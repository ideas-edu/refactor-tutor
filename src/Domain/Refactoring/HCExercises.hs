------------------------------------------------------------------
-- 
--
-- Created: 15-2-2019
--
------------------------------------------------------------------

module Domain.Refactoring.HCExercises where

import Domain.Evaluator (TestCase, makeTC)
import Domain.Syntax
import Domain.Refactoring.Exercise
import Utils.FileUtils
import Utils.ExerciseUtils
import Utils.ExerciseLoading
import Ideas.Common.Library as Ideas
import System.FilePath
import System.Directory (getCurrentDirectory)
import Data.Maybe

exerciseFolder :: FilePath
exerciseFolder = "exercises"

sumValuesExercise, sumValuesExerciseWeb, oddSumExerciseWeb :: IO (Maybe (Exercise ClassMember))
sumValuesExercise    = loadHCExercise "2.sumValues" ("..//exercises" </> "sumValues") testCasesSumValues
sumValuesExerciseWeb = loadHCExercise "2.sumValues" (exerciseFolder </> "sumValues") testCasesSumValues

oddSumExerciseWeb    = loadHCExercise "3.oddSum"  (exerciseFolder </> "oddSum") oddSumTestCases

doubleExercise, doubleExerciseWeb :: IO (Maybe (Exercise ClassMember))
doubleExercise    = loadHCExercise "5.double" ("..//exercises" </> "double") testCasesDouble
doubleExerciseWeb = loadHCExercise "5.double" (exerciseFolder </> "double") testCasesDouble

evenExercise, evenExerciseWeb :: IO (Maybe (Exercise ClassMember))
evenExercise    = loadHCExercise "1.even" ("..//exercises" </> "even") evenCountTestCases
evenExerciseWeb = loadHCExercise "1.even" (exerciseFolder </> "even") evenCountTestCases

scoreExercise, scoreExerciseWeb :: IO (Maybe (Exercise ClassMember))
scoreExercise    = loadHCExercise "4.score" ("..//exercises" </> "score") scoreTestCases
scoreExerciseWeb = loadHCExercise "4.score" (exerciseFolder </> "score") scoreTestCases

freeExercise, freeExerciseWeb :: IO (Maybe (Exercise ClassMember))
freeExercise    = loadHCExercise "6.haveThree" ("..//exercises" </> "haveThree") have3TestCases
freeExerciseWeb = loadHCExercise "6.haveThree" (exerciseFolder </> "haveThree") have3TestCases

sigcseExercise :: IO (Maybe (Exercise ClassMember))
sigcseExercise = loadHCExercise "sigcse-demo" (exerciseFolder </> "sigcse-demo") oddSumTestCases

loadHCExercise :: String -> FilePath -> [TestCase] -> IO (Maybe (Exercise ClassMember))
loadHCExercise s loc tests = do
    curDir <- getCurrentDirectory
    let svDir = curDir </> loc
    exDesc    <- loadExDescription svDir
    startCode <- readClassMember (svDir </> "start.java")
    return $ do
        cm <- startCode
        return $ makeRefactorExercise
                    emptyRefExInput { testCases = tests, startCM = cm, models = [] }
                    refEx { uid = newId s, exDescription = exDesc }   -- newId "hc" # 

sumValuesCode :: IO ClassMember
sumValuesCode  = do
    curDir <- getCurrentDirectory
    let svDir = curDir </> "..//exercises" </> "sumValues"
    fromJust <$> readClassMember (svDir </> "start.java")

-- | Test cases for the SumValues exercise
testCasesSumValues :: [TestCase]
testCasesSumValues = map makeTC
    [ ([makeIntArray [1,2,3,4,-5], tLit], iLit 10)
    , ([makeIntArray [1,2,3,4,-5], fLit], iLit 5)
    , ([makeIntArray [-1,2,-3,4,-5], fLit], iLit (-3))
    , ([makeIntArray [-1,2,-3,4,-5,6], tLit], iLit (12))
    , ([makeIntArray [], tLit], iLit 0)
    , ([makeIntArray [-9], tLit], iLit 0)
    ]

oddSumCode :: IO ClassMember
oddSumCode  = do
    curDir <- getCurrentDirectory
    let svDir = curDir </> "..//exercises" </> "oddSum"
    fromJust <$> readClassMember (svDir </> "start.java")

-- | Test cases for the OddSum exercise
oddSumTestCases :: [TestCase]
oddSumTestCases = map makeTC
    [ ([makeIntArray [44, 12, 20, 1, -1, 3, 5,-1, 99, 4]], iLit 16)
    , ([makeIntArray []], iLit 0 )
    , ([makeIntArray [1]], iLit 0)
    --, ([makeIntArray [1, 2, 3, -2, 3, 9 ]], iLit 9)
    , ([makeIntArray [1,  -1 ]], iLit 0 )
    , ([makeIntArray [1, 2, 3, 4, 5]], iLit 6 )
    , ([makeIntArray [ 1, 2, 3, 0, 4, 5, 6, -1]], iLit 7 )
    --, ([makeIntArray [2, -3, 2, 3, -1, 5, 1]], iLit 5 )
    ]

testCasesDouble :: [TestCase]
testCasesDouble = map makeTC
    [ ([dLit 1000.0, iLit 4], iLit 18)
    -- , ([iLit 25, iLit 8], iLit 1) --!
    , ([dLit 1.0, iLit 10], iLit 8)
    , ([dLit 25.0, iLit 100], iLit 1) 
    , ([dLit 25.0, iLit 2], iLit 36) 
    , ([dLit 22.5, iLit 8], iLit 10) 
    , ([dLit 999, iLit 1], iLit 70) 
    ]

evenCountTestCases :: [TestCase]
evenCountTestCases = map makeTC
    [ ([makeIntArray [1, 2, 3, 4, 5]], iLit 2)
    , ([makeIntArray [2, 0, 4]], iLit 3 )
    , ([makeIntArray []], iLit 0 )
    , ([makeIntArray [2]], iLit 1)
    , ([makeIntArray [1, 3, 5, 7]], iLit 0 )
    , ([makeIntArray [22, 33, 1, 4, 8, 0, 88, 8, 1]], iLit 6 )
    ]

scoreCode :: IO ClassMember
scoreCode  = do
    curDir <- getCurrentDirectory
    let svDir = curDir </> "..//exercises" </> "score"
    fromJust <$> readClassMember (svDir </> "start.java")

scoreTestCases :: [TestCase]
scoreTestCases = map makeTC
    [ ([iLit 2, iLit 3], iLit 5)
    , ([iLit 1, iLit 4], iLit 6)
    , ([iLit 5, iLit 1], iLit 2) 
    , ([iLit 2, iLit 7], iLit 8) 
    , ([iLit 0, iLit 6], iLit 10) 
    , ([iLit 0, iLit 5], iLit 7) 
    , ([iLit 10, iLit 1], iLit (-3)) 
    ]

have3TestCases :: [TestCase]
have3TestCases = map makeTC
    [ ([ makeIntArray [3, 1, 3, 1, 3] ], tLit)
    , ([ makeIntArray [3, 1, 3, 3] ], fLit)
    , ([ makeIntArray [3, 4, 3, 3, 4] ], fLit)
    , ([ makeIntArray [1, 3, 1, 3, 1, 2]], fLit)
    , ([ makeIntArray [1, 3, 1, 3, 1, 3]], tLit)
    , ([ makeIntArray [1, 3, 3, 1, 3]], fLit)
    , ([ makeIntArray [1, 3, 1, 3, 1, 3, 4, 3]], fLit)
    , ([ makeIntArray [3, 4, 3, 4, 3, 4, 4]], tLit)
    , ([ makeIntArray [3, 3, 3]], fLit)
    , ([ makeIntArray [1, 3]], fLit)
    , ([ makeIntArray [3]], fLit)
    , ([ makeIntArray [1]], fLit)
    , ([ makeIntArray [] ], fLit)
    ]
