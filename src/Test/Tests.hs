{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.TestManager
-- import Test.Framework.CmdlineOptions 
import Test.Framework.TestTypes
import Data.Maybe
import System.Exit (ExitCode)
import {-@ HTF_TESTS @-} Test.Domain.Parsers.JavaParserTest
import {-@ HTF_TESTS @-} Test.Domain.EvaluatorTest
import {-@ HTF_TESTS @-} Test.Domain.EqualityTest
import {-@ HTF_TESTS @-} Test.Domain.DependencyTest
import {-@ HTF_TESTS @-} Test.Domain.Refactoring.RewriteRulesTest
import {-@ HTF_TESTS @-} Test.Domain.Refactoring.Rules.BuggyRules
-- import {-@ HTF_TESTS @-} Test.Domain.Refactoring.StrategyTests -- uses logic tutor


main :: IO ()
main = htfMain htf_importedTests
