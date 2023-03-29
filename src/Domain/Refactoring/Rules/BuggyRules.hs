module Domain.Refactoring.Rules.BuggyRules(incrementAssignBuggy, compoundSubtractionBuggy) where

import Domain.Parsers.JavaParser

import Domain.Refactoring.Util
import Domain.Printer
import Domain.Syntax hiding ((./.))
import Domain.Views
import Ideas.Common.Library as Ideas hiding ((.%.))
import Ideas.Common.Rewriting.Confluence
import Data.Maybe
import Control.Applicative hiding (many)
import Control.Monad
import Domain.Terms

-- count = count++;
incrementAssignBuggy :: Rule Statement
incrementAssignBuggy = buggy $ ruleRewrite $ makeRewriteRule "incrementAssign" $
  \x -> ExprStat (x .=. (x .+. LiteralExpr (IntLiteral 1))) :~> ExprStat (x .=. Postfixed Incr x) 

-- score =- 3;
compoundSubtractionBuggy :: Rule Statement
compoundSubtractionBuggy = buggy $ ruleRewrite $ makeRewriteRule "compoundSubtractionBuggy" $
  \x n -> ExprStat (x .=. (x .-. n)) :~> ExprStat (x .=. (Prefixed Minus n))

