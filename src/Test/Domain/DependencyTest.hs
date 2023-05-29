{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Domain.DependencyTest where

import Domain.Dependency
import Domain.Parsers.JavaParser
import Domain.Syntax
import Domain.Base.AST
import Domain.Base.Conversion
import Test.TestUtils
import Test.Framework
import Test.Framework.TestInterface (Assertion)
import Data.Maybe

parseStat' = head . getBlockStats . body . forceParseFragment

{-
-- broken
test_deps :: Assertion
test_deps = do
    assertBool $ parseStat' "break;" `dependsOn` parseStat' "break;"
    assertBool $ parseStat' "x=1;" `dependsOn` parseStat' "x=2;"
    
    -- assertBool $ not $ parseStat' "a;" `dependsOn` parseStat' "b;" -- does not parse
    assertBool $ parseStat' "if(1) a=x;" `dependsOn` parseStat' "if (1) x=2;"
    assertBool $ parseStat' "x[1] = 2;" `dependsOn` parseStat' "int [] x = new int [2];"
    assertBool $ parseStat' "y = x[0];" `dependsOn` parseStat' "int [] x = {1,2,3};" -- fails
    -- Print

    assertBool $ parseStat' "print(x);" `dependsOn` parseStat' "print(x);"
    assertBool $ parseStat' "print(x);" `dependsOn` parseStat' "x=4;"
    assertBool $ parseStat' "x=1;" `dependsOn` parseStat' "print(x);"
    assertBool $ not $ parseStat' "print(x);" `dependsOn` parseStat' "a=x;" 
    assertBool $ parseStat' "if(1) print(1);" `dependsOn` parseStat' "if(2) print(2);"
    assertBool $ parseStat' "{ print(x); }" `dependsOn` parseStat' "{ a=1;x=2;b=1;}"
-}

{-
-- broken
test_depsBase :: Assertion
test_depsBase = do
    assertEqual [] (usesIds $ parseBS "x=1;") 
    assertEqual [makeIdentifier "a"] (usesIds $ parseBS "x=a;")  
    assertListsEqualAsSets (idts ["a", "b"]) (usesIds $ parseBS "x=a + b;") 

    assertEqual (idts ["a", "b", "c", "d"]) (usesIds $ parseBS "if (a) while (b) { f(c); g(d); } ")

    assertEqual (idts ["a", "i", "j"]) (usesIds $ parseBS "x[i+j] = a;")
    where
        idt = makeIdentifier
        idts = map makeIdentifier
-}