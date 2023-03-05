module Validation.Validation 
(
    runPHPValidation, runJavaValidation, runValidations
)
where


import Domain.Parsers.JavaParser
import Domain.Syntax
import Domain.Strategy.Exercise
import Domain.Evaluator
import Domain.Strategy.StrategyGenerator
import Domain.Equality
import Ideas.Common.Strategy (rulesInStrategy)
import Ideas.Common.Library (liftToContext)
import Ideas.Common.Exercise
import System.Directory
import Data.List
import Data.Either
import Control.Monad
import Control.Applicative
import System.FilePath ((</>))
import System.CPUTime (getCPUTime)
import Text.Printf
import Data.Maybe
import Test.TestUtils
import Ideas.Service.State
import Ideas.Service.Diagnose
import FPTutor.Diagnose

relPathPHP, relPathJava:: FilePath
relPathPHP = "..\\..\\..\\Validation\\Opdrachten webprogrammeren"
relPathJava =  "..\\..\\..\\Validation\\Java opdrachten"

foldersPHP, foldersJava :: [String]
foldersPHP = ["opdracht1","opdracht2", "opdracht3", "opdracht4"]
foldersJava = ["opdracht0", "opdracht1", "opdracht2"]


-- | Performs an analysis of a specific PHP exercise (1-4) 
runPHPValidation:: Int -> Bool -> IO ()
runPHPValidation nr debug
    | nr >= 1 && nr <= 4    = validateEx phpLang (relPathPHP </> foldersPHP!!(nr - 1)) debug
    | otherwise             = putStrLn "Invalid exercise" 

-- | Performs an analysis of a specific Java exercise (1-2) 
runJavaValidation:: Int -> Bool -> IO ()
runJavaValidation nr debug
    | nr >= 0 && nr <= 2    = validateEx javaLang (relPathJava </> foldersJava!!nr) debug
    | otherwise             = putStrLn "Invalid exercise" 

-- | Runs all validations     
runValidations:: Bool -> IO ()
runValidations debug =     
    do
         mapM_ (\folder -> validateEx phpLang (relPathPHP </> folder) debug) foldersPHP
         mapM_ (\folder -> validateEx javaLang (relPathJava </> folder) debug) foldersJava
         --return ()

validateEx:: Exs -> FilePath -> Bool -> IO ()
validateEx lang folder debug = 
    do
        start <- getCPUTime
        
        let modelPath = folder </> "model\\"
            studentPath = folder </> "student\\"
            suffix = extension lang
        
        putStrLn $ "Checking dir: " ++ show folder
        
        -- Models
        modelFiles <- getFilesWithSuffix modelPath suffix
        putStrLn $ show (length modelFiles) ++ " model files found"
        
        modelResults <- mapM (checkFile (parser $ ex lang) modelPath) modelFiles  -- [Analysis]   
        
        -- Make strategy
        let parsedModels = filter isParsed modelResults 
            strat = genProgramStrategy 1 . rights . map parsed $ parsedModels
            modelOuts = rights . nub . map evalProgram . solutions . liftToContext $ strat     
       
        putStrLn $ "Parsed " ++ show (length parsedModels) ++ "/" ++ show (length modelFiles) 

        -- Student programs
        studentFiles <- getFilesWithSuffix studentPath suffix
        putStrLn $ show (length studentFiles) ++ " student files found"
        
        studentResults <- mapM (checkFile (parser $ ex lang) studentPath) studentFiles   
            
        let parsedRes   = filter isParsed studentResults  
            evalResults = filter isEval studentResults
            evalErrors  = filter (not . isEval) studentResults
        
        putStrLn $ "Rules in strat:"  ++ show (length $ rulesInStrategy strat)
        
        let ex = makeAnonExercise lang (rights $ map parsed parsedModels) 
        
        let isExpect = checkDiag ex 
            isSol = checkSol strat
        let allActions = isSol . compareToModelOut modelOuts
            newS = map allActions parsedRes 
        
        putStrLn $ "Parsed " ++ show (length parsedRes) ++ "/" ++ show (length studentFiles) ++ 
            ", successfully evaluated " ++ show (length evalResults) ++ "/" ++ show (length parsedRes) ++
            ", considered correct by output " ++ show (length $ filter isEqMod newS) 
              ++ ", follow a known strategy " ++ show (length $ filter isByStrat newS)
             -- ++ ", expected " ++ show (length $ filter isExp newS)
        
        -- names of recognised files   
        showRecognisedFiles newS
        
        when debug $ putStrLn . unlines . map show . sort $ newS
        
        -- time 
        end <- getCPUTime
        let diff = (fromIntegral (end - start)) / (10^12)
        _ <- printf "Computation time: %0.3f sec\n" (diff :: Double)
    
        return ()
            where
                checkSol strat an = an { byStrategy = isSolution <$> (Just strat) <*> (getP an) }
                checkDiag ex an = an { expected = diag ex <$> getP an }
                compareToModelOut modelOuts an = an { equalToModel = Just $ any (flip hasSol an) modelOuts }
                showRecognisedFiles = putStrLn . unlines . map fileName . filter isByStrat

getFilesWithSuffix path suffix = do
    studentFiles <- getDirectoryContents path   
    return $ filter (isSuffixOf suffix) studentFiles 

-- return an analysis with parse + evaluation     
checkFile:: (String -> Either String Program) -> String -> FilePath -> IO Analysis      
checkFile parser folder file  = 
    do 
        fileS <- readFile $ folder </> file
        let an = makeAnalysis { fileName = file, parsed = parser fileS }
        let e = either (const $ Left $ EvalError "not evaluated") evalProgram $ parsed an  
        return an { eval = e }
        
                
--listFiles = do
--    files <- mapM (\f -> getDirectoryContents $ relPath ++ f ++ "student" )  folders
 --   putStrLn $ unlines $ concat files


success (Left _ ) = False
success (Right _ ) = True


getP an = either (const Nothing) Just $ parsed an

-- Analysis data type
data Analysis = Analysis { 
    fileName        :: String, 
    parsed          :: Either String Program,
    eval            :: Either EvalError String,
    equalToModel    :: Maybe Bool,
    similarToModel  :: Maybe Bool,
    byStrategy      :: Maybe Bool, -- isSol
    expected      :: Maybe Bool -- diagnose
} deriving Eq

instance Show Analysis where
    show an = fileName an ++ ", " ++ 
        either (\err ->  "Parse ERR" ++ err) (const "Parsed OK") (parsed an) ++ ", \n" ++
        either (\err -> show err) id (eval an) ++ ", \n" ++
        maybe "Unknown" (("Equal to output: " ++) . show) (equalToModel an) ++ ", " 
        ++ maybe "Unknown" (("By strategy: " ++) . show) (byStrategy an) ++ "\n" 
        -- ++ maybe "Unknown" (("Expected " ++) . show ) (expected an) ++ "\n" 
         
makeAnalysis:: Analysis         
makeAnalysis = Analysis { 
    fileName        = "", 
    parsed          = Left "not parsed yet",
    eval            = Left (EvalError "not evaluated yet"),
    equalToModel    = Nothing,
    similarToModel  = Nothing,
    byStrategy      = Nothing,
    expected      = Nothing
} 

instance Ord Analysis where
    compare Analysis { parsed = (Right _), eval = (Right _) } _ = LT
    compare Analysis { parsed = (Right _), eval = (Left _) } _ = EQ    
    compare _ _ = GT 
    
isParsed, isEval, isByStrat, isExp, isEqMod :: Analysis -> Bool

isParsed (Analysis { parsed = Right _}) = True
isParsed _ = False

isEval (Analysis { eval = Right _}) = True
isEval _ = False

isByStrat = isJustTrue . byStrategy 

isExp  = isJustTrue . expected

isEqMod = isJustTrue . equalToModel

isJustTrue :: Maybe Bool -> Bool
isJustTrue (Just True) = True
isJustTrue _ = False

hasSol modelout (Analysis { eval = Right out }) = out == modelout 
hasSol _ _ = False

showParseErrors ans = map (\ Analysis{ parsed = Left err, fileName = f } -> f ++ ": " ++ err) 
    $ filter (not . isParsed) ans

diag :: Exercise Program -> Program -> Bool
diag ex = isExpected . deepdiagnose state . inContext ex 
   where state = emptyState ex makeEmptyProgram 
   
