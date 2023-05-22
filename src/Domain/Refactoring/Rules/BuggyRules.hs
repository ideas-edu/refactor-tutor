module Domain.Refactoring.Rules.BuggyRules(incrementAssignBuggy, decrementAssignBuggy, compoundSubtractionBuggy, compoundAdditionBuggy, forToForeachBuggy) where

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

-- | Buggy rule for Refactoring Misconception A3. BadArithmeticExpressionShortening
-- | `count = count + 1;` ~> `count = count++;`
incrementAssignBuggy :: Rule Statement
incrementAssignBuggy = buggy $ ruleRewrite $ makeRewriteRule "incrementAssignBuggy" $
  \x -> ExprStat (x .=. (x .+. LiteralExpr (IntLiteral 1))) :~> ExprStat (x .=. Postfixed Incr x) 

-- | Buggy rule for Refactoring Misconception A3. BadArithmeticExpressionShortening
-- | `score = score - 1;` ~> `score = score--;`
decrementAssignBuggy :: Rule Statement
decrementAssignBuggy = buggy $ ruleRewrite $ makeRewriteRule "decrementAssignBuggy" $
  \x -> ExprStat (x .=. (x .-. LiteralExpr (IntLiteral 1))) :~> ExprStat (x .=. Postfixed Decr x) 

-- | Buggy rule for Refactoring Misconception A3. BadArithmeticExpressionShortening
-- | `score = score - 3;` ~> `score =- 3;`
compoundSubtractionBuggy :: Rule Statement
compoundSubtractionBuggy = buggy $ ruleRewrite $ makeRewriteRule "compoundSubtractionBuggy" $
  \x n -> ExprStat (x .=. (x .-. n)) :~> ExprStat (x .=. (Prefixed Minus n))

-- | Buggy rule for Refactoring Misconception A3. BadArithmeticExpressionShortening
-- | `score = score + 1;` ~> `score =+ 1`
compoundAdditionBuggy :: Rule Statement
compoundAdditionBuggy = buggy $ ruleRewrite $ makeRewriteRule "compoundAdditionBuggy" $
  \x n -> ExprStat (x .=. (x .+. n)) :~> ExprStat (x .=. (Prefixed Plus n))

{-
-- Old, but has (almost) the same behaviour as the new view based one below
forToForeachBuggy :: Rule Statement
forToForeachBuggy = buggy $ ruleRewrite $ makeRewriteRule "forToForeachBuggy" $
  \i arr b -> For (ForInitDecls IntType [Assignment Assign (IdExpr i) (LiteralExpr (IntLiteral 0))]) [Infixed Less (IdExpr i) (Property arr (Identifier {name = "length"}))] [Postfixed Incr (IdExpr i)] b :~> ForEach IntType i (IdExpr arr) b
-}

-- | Buggy rule for incorrectly transforming an for loop into a foreach by forgetting to update the body. This rule will only work if the body in both the for and foreach are the same.
forToForeachBuggy :: Rule Statement
forToForeachBuggy = buggy $ makeRule "forToForeachViewBuggy" f'
  where
    f' :: Statement -> [Statement]
    f' (For fInit [fCond] [fIncr] fBody) = do
      c            <- maybeToList $ match viewInitZero fInit
      (c2, fArr)   <- maybeToList $ match viewToArrLength fCond
      c3           <- maybeToList $ match viewIncrOne fIncr
      
      if c == c2 && c2 == c3
        then map (\t -> ForEach t c (IdExpr fArr) fBody) allDataTypes
        else []
    f' _ = []

