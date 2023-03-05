

module Domain.Javac where

import Domain.Syntax
import Domain.Parsers.JavaParser
import Domain.Printer
import System.Process
import System.FilePath
import System.Directory
import System.Exit
import qualified Data.ByteString.Lazy as BS
import Data.List
-- import Data.String.Utils
import Data.Either
import Control.Monad
import Control.Exception
import Data.Maybe

javacLocal = "C:\\Program Files\\Java\\jdk-10.0.2" </> "bin"</> "javac" <.> "exe"
javacLocalWeb = "jdk-10.0.2" </> "bin"</> "javac" <.> "exe" 
javacServer = "javac"

runJavac :: JavaCode -> String -> IO (Either String (Maybe [String]))
runJavac code userid = do
    options <- mapM (\p -> runJavacAt p code userid) [javacServer, javacLocalWeb, javacLocal]
    return $ fromMaybe (Left . head . lefts $ options) $ find isRight options

runJavacAt :: FilePath -> JavaCode -> String -> IO (Either String (Maybe [String]))
runJavacAt javacExe javaCode userid = do

    curDir <- getCurrentDirectory
    writeFile (javaFile curDir) (insertCodeInClassWith className javaCode)

    let options = [javaFile curDir]
    (exitCode, out, err) <- readProcessWithExitCode javacExe options []
    -- removeFile (javaFile curDir)
    case exitCode of
        ExitSuccess   -> removeFile (classFile curDir) >> return (Right Nothing)
        ExitFailure 1 -> return . Right . Just . lines $ err
        _             -> return . Left $ "Unknown Error (" ++ err ++ ")(" ++ out ++ ")"

    `catch` \e -> const (return $ Left "IO Exception") (e :: IOException)
    where
        javaFile curDir  = curDir </> className <.> "java"
        classFile curDir = curDir </> className <.> "class"
        className        = "X" ++ userid

processJavacOutput :: Either String (Maybe [String]) -> String
processJavacOutput jo = case jo of
    Right (Just errs) -> show (nrErrors errs) ++ " compiler error(s), first @ line " ++ processFileErr (head errs)
    Right Nothing     -> "Compiled successfully"
    Left e            -> e
    where
        nrErrors xs = max 1 (length xs `div` 5) -- sometimes 0

        processCErr [fileErr, line, arrow, symb, loc] = 
            "err " ++ symb
        processFileErr = (!!1) . split ".java:"

hasJavacOutput :: Either String (Maybe [String]) -> Bool
hasJavacOutput (Right (Just _)) = True
hasJavacOutput _                = False

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim
                                                 (drop (length delim) x)
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

{- | Similar to Data.List.break, but performs the test on the entire remaining
list instead of just one element.
-}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)
