{-# LANGUAGE NamedFieldPuns #-}
module Domain.Refactoring.Rules.BuggyRules(incrementAssignBuggy, decrementAssignBuggy, compoundSubtractionBuggy, compoundAdditionBuggy, removeIfEqualsLitViewBuggy) where

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
import Debug.Trace

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

removeIfEqualsLitBuggy :: [Rule Statement]
removeIfEqualsLitBuggy = map buggy $ map ruleRewrite $ makeRewriteRules "removeEqualsLitBuggy" $
  [ \i b  _  -> If (Infixed Equal i (LiteralExpr (BoolLiteral False))) b :~> If i b
  , \i bt bf -> IfElse (Infixed Equal i (LiteralExpr (BoolLiteral False))) bt bf :~> IfElse i bt bf
  , \i b  _  -> If (Infixed NotEqual i (LiteralExpr (BoolLiteral True))) b :~> If (Prefixed Not i) b
  , \i bt bf -> IfElse (Infixed NotEqual i (LiteralExpr (BoolLiteral True))) bt bf :~> IfElse (Prefixed Not i) bt bf
  ]

removeIfEqualsLitViewBuggy :: Rule ClassMember
removeIfEqualsLitViewBuggy = buggy $ makeRule "removeIfEqualsLitViewBuggy" f'
  where
    f' :: ClassMember -> [ClassMember]
    f' m@Method {mBody} = do
      let (Fragment _ stats) = mBody
          perms = boolPerms (length stats)
          pairs = zip perms (repeat stats)
          pairs' = map (uncurry zip) pairs
          stats' = (map . map) (\(b, s) -> if b then tryApply removeIfEqualsLitBuggy s else s) pairs'
          perms' = map (\s' -> m {mBody = Fragment 0 s'}) stats'
      filter (/= m) perms'
    f' _ = []

    tryApply :: [Rule Statement] -> Statement -> Statement
    tryApply [] s = s
    tryApply (r:rs) s = case apply r s of
      Just s' -> s'
      Nothing -> tryApply rs s

boolPerms :: Int -> [[Bool]]
boolPerms n | n < 0 = error "n must be greater or equal to 0"
boolPerms n = boolPerms' n [[]]
  where
    boolPerms' 0 xs = xs
    boolPerms' n xs = boolPerms' (n-1) (f True xs ++ f False xs)

    f b x = map (b :) x

testMethod = Method {mReturnType = Nothing, mId = Identifier {name = "main"}, mParams = [], mBody = Fragment {fLocId = 0, stats = [ExprStat (Call (Identifier {name = "print"}) []),If (Infixed Equal (IdExpr (Identifier {name = "c"})) (LiteralExpr (BoolLiteral True))) (Block 0 [Return (Just (LiteralExpr (BoolLiteral True)))]),ExprStat (Call (Identifier {name = "print"}) []),IfElse (Infixed Equal (IdExpr (Identifier {name = "c"})) (LiteralExpr (BoolLiteral True))) (Block 0 [Return (Just (LiteralExpr (BoolLiteral False)))]) (Block 0 [Return (Just (LiteralExpr (BoolLiteral True)))])]}, mMdfs = []}
