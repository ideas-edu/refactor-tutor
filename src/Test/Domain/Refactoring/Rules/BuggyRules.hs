{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Domain.Refactoring.Rules.BuggyRules where

import Test.TestUtils
import Test.Framework
import Test.Framework.TestInterface (Assertion)
import Domain.Refactoring.Rules.Rules
import Domain.Refactoring.Rules.LoopRules
import Domain.Refactoring.Rules.BuggyRules
import Domain.Transformation.ProgramTransformation
import Domain.Base.Normalisation
import Domain.Syntax
import Domain.Parsers.JavaParser
import Ideas.Common.Library

test_incrementAssignBuggy :: Assertion
test_incrementAssignBuggy = assertJustEq after $ apply incrementAssignBuggy before
  where
    before = ExprStat (Assignment Assign (IdExpr (Identifier {name = "x"})) (Infixed Addition (IdExpr (Identifier {name = "x"})) (LiteralExpr (IntLiteral 1))))
    after = ExprStat (Assignment Assign (IdExpr (Identifier {name = "x"})) (Postfixed Incr (IdExpr (Identifier {name = "x"}))))

test_decrementAssignBuggy :: Assertion
test_decrementAssignBuggy = assertJustEq after $ apply decrementAssignBuggy before
  where
    before = ExprStat (Assignment Assign (IdExpr (Identifier {name = "x"})) (Infixed Subtraction (IdExpr (Identifier {name = "x"})) (LiteralExpr (IntLiteral 1))))
    after = ExprStat (Assignment Assign (IdExpr (Identifier {name = "x"})) (Postfixed Decr (IdExpr (Identifier {name = "x"}))))

test_compoundSubtractionBuggy :: Assertion
test_compoundSubtractionBuggy = assertJustEq after $ apply compoundSubtractionBuggy before
  where
    before = ExprStat (Assignment Assign (IdExpr (Identifier {name = "x"})) (Infixed Subtraction (IdExpr (Identifier {name = "x"})) (LiteralExpr (IntLiteral 3))))
    after = ExprStat (Assignment Assign (IdExpr (Identifier {name = "x"})) (Prefixed Minus (LiteralExpr (IntLiteral 3))))

test_compoundAdditionBuggy :: Assertion
test_compoundAdditionBuggy = assertJustEq after $ apply compoundAdditionBuggy before
  where
    before = ExprStat (Assignment Assign (IdExpr (Identifier {name = "x"})) (Infixed Addition (IdExpr (Identifier {name = "x"})) (LiteralExpr (IntLiteral 3))))
    after = ExprStat (Assignment Assign (IdExpr (Identifier {name = "x"})) (Prefixed Plus (LiteralExpr (IntLiteral 3))))

test_forToForeachBuggy :: Assertion
test_forToForeachBuggy = assertJustEq after $ apply forToForeachBuggy before
  where
    before = For (ForInitDecls IntType [Assignment Assign (IdExpr (Identifier {name = "i"})) (LiteralExpr (IntLiteral 0))]) [Infixed Less (IdExpr (Identifier {name = "i"})) (Property (Identifier {name = "items"}) (Identifier {name = "length"}))] [Postfixed Incr (IdExpr (Identifier {name = "i"}))] (Block 0 [ExprStat (Call (Identifier {name = "print"}) [ArrayAcc (Identifier {name = "items"}) (IdExpr (Identifier {name = "i"}))])])
    after = ForEach BoolType (Identifier {name = "i"}) (IdExpr (Identifier {name = "items"})) (Block 0 [ExprStat (Call (Identifier {name = "print"}) [ArrayAcc (Identifier {name = "items"}) (IdExpr (Identifier {name = "i"}))])])
