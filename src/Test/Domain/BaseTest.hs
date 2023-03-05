{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Domain.BaseTest where

import Domain.Base.AST
import Test.Framework
import Test.Framework.TestInterface (Assertion)

test_base :: Assertion
test_base = do
    assertEqual (parseB "x++;") (parseB "x++;")