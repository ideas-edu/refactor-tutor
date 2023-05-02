{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Domain.Parsers.JavaParserTest where

import Domain.Parsers.JavaParser
import Domain.Syntax
import Domain.TestPrograms
import Domain.Printer
import Domain.Base.Conversion
import Utils.Utils
import Domain.Equality ((~~))

import Data.List
import Data.Either
import Data.Maybe

import Control.Applicative
import Control.Monad

import Test.Framework.TestInterface (Assertion)
import Test.Framework

import System.Directory
import System.FilePath ((</>), takeDirectory)

import Language.Java.Syntax (CompilationUnit)
import Language.Java.Pretty (prettyPrint)
import Text.Parsec.Error (ParseError)

--------------------------------------------------------------------------------
-- Manual tests

parseClassFileToInternal :: FilePath -> IO ()
parseClassFileToInternal file = readFile file >>= parseJCode False 

parseFile :: FilePath -> IO ()
parseFile file = readFile file >>= parseJCode False 

-- | No need for full path
parseTestFile :: FilePath -> IO ()
parseTestFile name = readTestFile name >>= parseJCode False

parseTestFilePretty :: FilePath -> IO ()
parseTestFilePretty name = readTestFile name >>= parseJCode True

parseFilePretty :: FilePath -> IO ()
parseFilePretty file = readFile file >>= parseJCode True

parseJCode :: Bool -> JavaCode -> IO ()
parseJCode pretty code = putStrLn $ 
    case parseClass code of 
        Left err -> err
        Right c  -> (if pretty then showPretty else show) c


--------------------------------------------------------------------------------
-- Tests

test_parseJavaPrograms :: Assertion
test_parseJavaPrograms = 
    do  
        javaFiles <- collectJavaTestFileNames       
        putStrLn $ "Nr of Java programs found: " ++ show (length javaFiles)    
        results <- mapM parseFile javaFiles
        assertEmpty (filter isLeft results)
    where
        parseFile name = do
            code <- readTestFile name
            let result = parseClass code
            putStrLn $ case result of
                Left err    -> "Error in " ++ name ++ ": " ++ err
                Right _     -> name ++ " ok"
            return result

-- | Tests if the AST of a program is equal to the AST that converted to 
-- the internal AST, pretty-printed, and parsed again 
test_javaConverter :: Assertion
test_javaConverter = do   
    javaFiles <- collectJavaTestFileNames       
    putStrLn $ "Nr of Java programs found: " ++ show (length javaFiles)    
    results <- mapM testConv javaFiles
    mapM_ putStrLn (catMaybes results)
    assertEmpty (catMaybes results)
    where
        -- returns Nothing or an error string
        testConv :: String -> IO (Maybe String)
        testConv name = do
            code <- readTestFile name
            putStrLn $ "Checking file " ++ name
            res <- case fmap showPretty (parseClass code) of 
                Left errS -> return $ Just "Error in converting code: " -- ++ show errS)
                Right p -> case (parseEJava p currentTemplate, parseEJava code currentTemplate) of
                                (Right l, Right r) -> do
                                    --putStrLn (prettyPrint l)
                                    --putStrLn (prettyPrint r)
                                    return $ if l == r then Nothing else Just "Not equal"
                                (Left l, _) -> return $ Just ("Error in converted code: " ++ show l)
                                (_, Left l) -> return $ Just ("Error in original code: " ++ show l)
            when (isJust res) $ putStrLn (fromJust res)
            return res           

test_javaParseErrors:: Assertion
test_javaParseErrors = do
    assertNothing $ parseClass' "?"
  
prop_parsePrettyPrintedJavaProgram:: Program -> Bool
prop_parsePrettyPrintedJavaProgram parsedCode = 
    toB parsedCode == (toB . fromJust . parseClass' . showPretty) parsedCode

--------------------------------------------------------------------------------
-- Test file utils

-- | Collects all Java file names in the Test-directory
collectJavaTestFileNames :: IO [String]      
collectJavaTestFileNames = do
    testFiles <- testFilesPath >>= getDirectoryContents  
    return $ filter (isSuffixOf ".java") testFiles 

-- | location of the test files
testFilesPath :: IO FilePath
testFilesPath = do    
    curDir <- getCurrentDirectory
    return $ curDir </> "test"

readTestFile :: String -> IO String
readTestFile name = do
    path <- testFilesPath
    readFile $ path </> name
