module Utils.ExerciseUtils where

import Ideas.Common.Library
import Utils.FileUtils

--------------------------------------------------------------------------------
-- Languages & settings

data ExerciseType = JavaEx | PHPEx | JavaOOEx | RefEx | UnknownEx deriving Show

data ExerciseSettings = ExerciseSettings
    {
          exType        :: ExerciseType
        , language      :: Language
        , exDescription :: String
        , feedbacklevel :: Int
        , uid           :: Id
    }  

instance Show ExerciseSettings where
    show es = "Settings " ++ unwords 
        [show (uid es), show (exType es), show (feedbacklevel es), exDescription es]

langExtension :: ExerciseSettings -> String
langExtension = extension . language

emptyExSettings :: ExerciseSettings
emptyExSettings = ExerciseSettings 
    {
          exType        = UnknownEx
        , language      = undefined
        , exDescription = "<no decription>"
        , feedbacklevel = 1
        , uid           = newId "<no id>"
    }

