module Domain.Refactoring.Exercise where

import Domain.Evaluator (TestCase)
import Domain.Syntax
import Domain.Printer
import Domain.Parsers.JavaParser
import Domain.Refactoring.Strategy
import Domain.Refactoring.Metrics
import Domain.Refactoring.Rules.Rules
import Domain.Refactoring.Rules.BuggyRules
import Domain.Refactoring.Equality
import Utils.FileUtils
import Utils.ExerciseUtils

import Ideas.Common.Library as Ideas

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe

data RefExInput = RefExInput
    {
          startCM   :: ClassMember
        , testCases :: [TestCase]
        , models    :: [ClassMember]
    }

refEx :: ExerciseSettings 
refEx = emptyExSettings { language = javaLang, exType = RefEx }

emptyRefExInput :: RefExInput
emptyRefExInput = RefExInput { startCM = undefined, testCases = [], models = [] }

makeRefactorExercise :: RefExInput -> ExerciseSettings -> Exercise ClassMember
makeRefactorExercise refExInput settings = addReady $ emptyExercise
      { -- identification and meta-information
        exerciseId     = describe (exDescription settings) $ uid settings, --"ref." # 
        -- parsing and pretty-printing
        parser         = join . fmap (maybe (Left "Code should only contain a single method") Right . stripMethod) . parseMethod,
        prettyPrinter  = ppJavaProgram,
        -- syntactic and semantic checks
        equivalence    = withoutContext (xx $ testCases refExInput), -- \_ _ -> true, 
        similarity     = (==), --withoutContext (~~),
        ready          = predicate (\_ -> false), 
        suitable       = true,
         -- strategies and rules
        strategy       = doAllS,
        navigation     = termNavigator,
        ruleOrdering   = \_ _ -> LT,
        hasTypeable    = useTypeable,
        properties     = Map.empty, 
        canBeRestarted = true,
        examples       = examplesWithDifficulty [(Medium, startCM refExInput)],
        constraints    = [] ++ map c2 (testCases refExInput), -- check in diagnose
        extraRules     = [use buggyEqualsTrue, use buggyCollapseIfR, use incrementAssignBuggy, use compoundSubtractionBuggy]
      }
    where
        addReady e = e { ready = predicate (stepsLeft e) }

makeTestRefExercise :: JavaCode -> Exercise ClassMember
makeTestRefExercise jc = 
    let method = fromJust . stripMethod . forceParseMethod $ jc
    in  makeRefactorExercise (emptyRefExInput { startCM = method }) refEx

deriv :: JavaCode -> IO ()
deriv jc = 
    let m = fromJust . stripMethod . forceParseMethod $ jc
        ex = makeTestRefExercise jc 
    in printDerivations ex m
