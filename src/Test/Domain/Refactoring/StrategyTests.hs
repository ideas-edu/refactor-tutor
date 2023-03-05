{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Domain.Refactoring.StrategyTests where

import Test.Framework
import Test.Framework.TestInterface (Assertion)
import Test.TestUtils
import Domain.Refactoring.ExpStrategy
import Domain.Syntax
import Domain.Parsers.JavaParser
import Domain.Refactoring.Util
import Ideas.Common.Library


-- Boolean expressions ----

test_dnf1 = assertJustEqC (parseEx "!p && !q") $
    applyInNewContext normBoolExp (parseEx "!(p || q)")
       
test_dnf2 = assertJustEqC (parseEx "p && q") $
    applyInNewContext normBoolExp (parseEx "p && (!p || q)")

test_dnf3 = assertJustEqC (parseEx "q || (p && r)") $
    applyInNewContext normBoolExp (parseEx "q || (p && r)")

test_dnf4 = assertJustEqC (parseEx "q || (!p || r)") $
    applyInNewContext normBoolExp (parseEx "!(!q && (p && !r))")

test_dnf5 = assertEqInCtxt normBoolExp "(p && !p) || (p || !p)" "true"

test_dnf6 = assertEqInCtxt normBoolExp "p || (p || !q)" "p || !q"

