------------------------------------------------------------------
-- Main
--
--
------------------------------------------------------------------

module Main where

import Utils.ExerciseLoading
import Domain.Refactoring.HCExercises
import Ideas.Service.DomainReasoner
import Ideas.Main.Default
import Ideas.Common.Library
import System.FilePath ((</>))
import RPTServices
import System.Directory (getCurrentDirectory)
import Ideas.Encoding.Options 
import Data.Maybe

main :: IO ()
main = drRef >>= defaultMainWith (mempty {maxTime = Just 20})

drRef :: IO DomainReasoner
drRef = 
    do
        curDir      <- getCurrentDirectory
        refExs      <- loadRefExercises (curDir </> "exercises" </> "")
        hcExs       <- sequence [] 
                    -- sigcseExercise, sumValuesExerciseWeb, oddSumExerciseWeb, evenExerciseWeb, scoreExerciseWeb, freeExerciseWeb, doubleExerciseWeb

        let exList = catMaybes hcExs ++ refExs
            dr' = describe "Domain reasoner for refactoring" (newDomainReasoner "RPT") {
                  exercises   = map Some exList 
                , services    = metaServiceList dr' ++ rptServiceList ++ serviceList
                , scripts     = myScripts (curDir </> "ref.txt") exList
            }
        return dr'

myScripts :: HasId a => FilePath -> [a] -> [(Id, FilePath)]
myScripts scriptFile = map (\e -> (getId e, scriptFile))
