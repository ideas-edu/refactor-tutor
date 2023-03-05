module Utils.ExerciseLoading where

import Domain.Refactoring.Exercise
import Domain.Syntax
import Domain.Evaluator (TestCase)
import Utils.TestParser (parseTest)
import Utils.FileUtils
import Utils.Utils
import Utils.ExerciseUtils

import Ideas.Common.Library

import Data.Maybe
import Control.Monad
import System.Directory
import System.FilePath

--------------------------------------------------------------------------------
-- Loading exercise files


            
loadExercisesFromFolder :: (FilePath -> IO (Maybe (Exercise a))) -> FilePath -> IO [Exercise a]
loadExercisesFromFolder f filePath = do
    dirExists <- doesDirectoryExist filePath
    if dirExists
        then 
            -- create an exercise for each subfolder, return only valid results
            liftM catMaybes $ listSubFolders filePath >>= mapM f
        else return []

-- | Loading a config file
-- Type is required, otherwise Nothing returned
loadConfig :: FilePath -> IO (Maybe ExerciseSettings)
loadConfig _ = return Nothing

loadExDescription :: FilePath -> IO String
loadExDescription dir = 
        readFileIfExists (combine dir "description.txt") 
    >>= return . fromMaybe "no description" 

-- TODO load test cases
loadRefExFromFolder :: FilePath -> IO (Maybe (Exercise ClassMember))
loadRefExFromFolder exFolder = 
    do
        -- load description
        exDesc <- loadExDescription exFolder
        -- load config    
        -- load tests
        tests <- loadTests exFolder
        -- load start program
        cm <- readClassMember (exFolder </> "start.java")
        let rei scm = emptyRefExInput { testCases = tests, startCM = scm } 
            sett    = refEx { uid = newId (takeFileName exFolder), exDescription = exDesc }
        return $ fmap (\scm -> makeRefactorExercise (rei scm) sett) cm

-- | Load all refactor exercises found in a given folder
loadRefExercises :: FilePath -> IO [Exercise ClassMember]
loadRefExercises = loadExercisesFromFolder loadRefExFromFolder

loadTests :: FilePath -> IO [TestCase]
loadTests fp = do
    testFile <- readFileIfExists (fp </> "tests.txt") 
    return $ maybe [] parseTests testFile
    where
        parseTests = catMaybes . map parseTest . lines 
    

