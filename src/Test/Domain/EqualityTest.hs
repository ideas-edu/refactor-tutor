{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Domain.EqualityTest where

import Domain.Equality
import Domain.Base.Normalisation
import Domain.Transformation.ProgramTransformation
import Domain.Base.Transformations
import Domain.Syntax
import Domain.Evaluator
import Domain.Base.AST
import Domain.Base.Conversion
import Domain.TestPrograms
import Test.Framework
--import Test.Framework.TestInterface (Assertion)
import Test.HUnit.Base
import Ideas.Common.Library as I
import Data.Maybe
import Domain.Parsers.JavaParser
import Data.Function (on)
import Test.TestUtils

  
-- normaliseF :: Program -> BProgram
normaliseF = fst . normalisePartial

(~~~) :: JavaCode -> JavaCode -> Bool
(~~~) = (~~) `on` parse

-- Assumes parsable input
parse :: JavaCode -> Program
parse = forceParseFragment

                 
-- the evaluation of a program should be equal to the evaluation of the normalised program            
--prop_evalNormalise:: Program -> Bool 
--prop_evalNormalise p = evalProgram p == evalProgram (normaliseF p)
          
prop_normaliseTwice:: Program -> Bool 
prop_normaliseTwice p = normaliseF p == x p
    where 
        x = normaliseF --  . normaliseF
        
-- a program should be similar to itself
prop_similarToSelf:: Program -> Bool
prop_similarToSelf p = p ~~ p

-- niet goed te checken
-- if 2 programs are similar they should also be equal
--prop_similarEq:: Program -> Program -> Property
--prop_similarEq p1 p2 = isSimilar p1 p2 ==> isEqual p1 p2

test_SPV11varNames :: Assertion
test_SPV11varNames = do
    assertBool       $ "if(x)if(y){print(y);if(x)print(x);}" ~~~ "if(a)if(b){print(b);if(a)print(a);}"
    assertBool . not $ "if(x)if(y){print(y);if(x)print(x);}" ~~~ "if(a)if(b){print(b);if(a)print(b);}"

-- Use functions instead of vars to avoid renaming
test_SPV57similarExpressions:: Assertion
test_SPV57similarExpressions = do
    assertBool $ "x=1+x;"                       ~~~ "x=x+1;"
    assertBool $ "x++;"                         ~~~ "x=x+1;"
    assertBool $ "x+=1+(2+0);"                  ~~~ "x=x +(1*3);"
    assertBool $ "x= y+(-x-(-x));"              ~~~ "x=y;"
    assertBool $ "a()+1+f()+b[4]+4+b[3]+s();"   ~~~ "s()+b[3]+a()+5+f()+b[4];"

    --logical
    assertBool $ "(!x)&&(!y);"          ~~~ "!(x||y);"
    assertBool $ "(!x)&&(!y)&&(!y);"    ~~~ "(!y)&&!(y||(x&&true));"
    assertBool $ "x==true;"             ~~~ "x;"
    
    -- compare
    assertBool $ "x()==y();"        ~~~ "y()==x();"
    assertBool $ "x()!=y();"        ~~~ "!(y()==x());"
    assertBool . not $ "x() = y();" ~~~ "y() = x();"
    assertBool $ "x()=y()==z();"    ~~~ "x()=z()==y();"
    
    --
    assertBool $ "(c() + a()) * (b() + a());"   ~~~ "(a() + b()) * (a() + c());"
    assertBool $ "(c()+a())-(b()+a());"         ~~~ "(a()+c())-(a()+b());"
    assertBool $ "(c()*a())-(b()*a());"         ~~~ "(a()*c())-(a()*b());"
    
    assertBool $ "(1>2);" ~~~ "(2<=1);"
    
    assertBool $ "a() * (b() + c());" ~~~ "(a() * b()) + (a() * c());"
   
--------------------------------------------------------------------------------
-- Copy propagation tests

assertEqAfterCP, assertNotEqAfterCP :: JavaCode -> JavaCode -> Assertion
assertEqAfterCP    before after = assertEqual    (parseB after) (copyProp (parseB before)) 
assertNotEqAfterCP before after = assertNotEqual (parseB after) (copyProp (parseB before)) 

assertSameAfterCP :: JavaCode -> Assertion
assertSameAfterCP before = assertEqAfterCP before before 

test_copyPropSimpleConst :: Assertion
test_copyPropSimpleConst =  do
    assertEqAfterCP "x=1;f(x);"                "x=1;f(1);"
    assertEqAfterCP "x=1;y=2;f(x+y);"          "x=1;y=2;f(1+2);"
    assertEqAfterCP "x=1;y=x;z=y+x;f(z);"      "x=1;y=1;z=1+1;f(1+1);"
    assertEqAfterCP "x=1;f(x);x=2;f(x);"       "x=1;f(1);x=2;f(2);"

test_copyPropSimple :: Assertion
test_copyPropSimple =  do
    assertEqAfterCP "x=a; f(x);"               "x=a; f(a);"
    assertEqAfterCP "x=a; y=x; f(y);"          "x=a; y=a; f(a);"
    assertEqAfterCP "x=a; y=x; x=b; f(x+y);"   "x=a; y=a; x=b; f(b+a);"

-- Do not copy prop vars defined in terms of itself  
test_copyPropCircular :: Assertion
test_copyPropCircular =  do
    assertSameAfterCP "x=x+1; x=x+2; f(x);"   
    assertSameAfterCP "x=x+1; g(x); x=x+3; g(x);"

test_copyPropIf :: Assertion
test_copyPropIf = do
    assertNotEqAfterCP "if(1) {x=5;}; f(x);"               "if(1) {x=5;}; f(5);"
    assertEqAfterCP    "if(1) x=5; else x=5;f(x);"         "if(1) x=5; else x=5;f(5);" -- ?
    assertEqAfterCP    "x=5;if(1) f();f(x);"               "x=5;if(1) f();f(5);" 
    assertSameAfterCP  "x=5;if(1) x=2;f(x);"          
    assertEqAfterCP    "x=5;if(1) a=x; else b=x; f(x);"    "x=5;if(1)a=5; else b=5; f(5);"
    assertSameAfterCP  "if(1) x=6; f(x);"
    assertSameAfterCP  "if(1) x=6; else x=5; f(x);"

test_copyPropLoop :: Assertion
test_copyPropLoop = do
    assertSameAfterCP  "x=1; while(x<5) {x=x+1;} y=x;" 
    assertSameAfterCP  "x=1; while(x<4) {if(2)x=x+1;} y=x;"
    assertEqAfterCP    "x=1; while(x<3) f(); y=x;"           "x=1; while(x<3) f(); y=1;"

    assertEqAfterCP    "while(b) {x=1; f(x); x=2; f(x);}"    "while(b) {x=1; f(1); x=2; f(2);}"
    assertSameAfterCP  "while(c) {f(x); x=2;}"         

    -- vars not changed in loop can be propagated
    assertEqAfterCP    "x=a; while(i) { f(x); } "            "x=a; while(i) { f(a); }"
    assertEqAfterCP    "x=a; while(i) { f(x); } g(x);"       "x=a; while(i) { f(a); } g(a);"  
    assertSameAfterCP  "x=a; while(i) { f(x); x=b; }" 
    assertEqAfterCP    "x=a; while(i) { f(x); x=b; f(x); }"  "x=a; while(i) { f(x); x=b; f(b); }"
    assertSameAfterCP  "x=a; while(i) { x=b; } f(x);" 

test_copyPropNest :: Assertion
test_copyPropNest = do
    assertEqAfterCP    "if(1){a=2;p(a);}"                  "if(1){a=2;p(2);}"
    assertEqAfterCP    "if(1){a=2;if(2) p(a);}"            "if(1){a=2;if(2) p(2);}"
    assertEqAfterCP    "x=3; while(1) if(1){p(x);}"        "x=3; while(1) if(1){p(3);}"
    assertNotEqAfterCP "x=3; while(1) if(1){p(x);x++;}"    "x=3; while(1) if(1){p(3);x++;}"    

-- TODO
test_copyPropExprAssigns :: Assertion
test_copyPropExprAssigns = do
    assertEqAfterCP    "x++;f(x);"                 "x++;f(x+1);" 
    assertEqAfterCP    "f(x++);g(x);"              "f(x++);g(x+1);"              

test_copyPropClass :: Assertion
test_copyPropClass = let parseClass = toB . forceParseClass
    in assertEqual (parseClass "class X { void x() {a=b;f(b);} void y() {a=c;f(c);} }")
         (copyProp (parseClass "class X { void x() {a=b;f(a);} void y() {a=c;f(a);} }"))

test_copyPropArrays :: Assertion
test_copyPropArrays = do 
    assertEqAfterCP    "x=a[i]; f(x);"       "x=a[i]; f(a[i]);"
    assertEqAfterCP    "x=a[i]; if(x) f(x);" "x=a[i]; if(a[i]) f(a[i]);"

    assertEqAfterCP    "a[i]=b; f(a[i]);"    "a[i]=b; f(b);"

    assertEqAfterCP    "while(i) { x=a[i]; b=x; }"  "while(i) { x=a[i]; b=x;}"
    -- assertFailure "<to do>"

--------------------------------------------------------------------------------
-- Copy prop for if(x==n)

-- todo remove fromJust
assertEqAfterCP2 :: JavaCode -> JavaCode -> Assertion
assertEqAfterCP2 before after = assertEqual (parseBS after) (fromJust $ copyPropIfCondition (parseBS before)) 

test_copyPropIfCondition :: Assertion
test_copyPropIfCondition =  do
    assertEqAfterCP2 "if(x==0) f(x);" "if(x==0) f(0);"
    assertEqAfterCP2 "if(0==x) f(x);" "if(0==x) f(0);"    
    assertEqAfterCP2 "if(x[i]==0) a=x[i];" "if(x[i]==0) a=0;"
    assertEqAfterCP2 "if(x[5]==0) a=x[5];" "if(x[5]==0) a=0;"

    assertEqAfterCP2 "if(x==1) f(x);" "if(x==1) f(1);" 
    assertEqAfterCP2 "if(x==\"\") f(x);" "if(x==\"\") f(\"\");" 

--------------------------------------------------------------------------------
-- Renaming tests

assertEqAfterRRV :: JavaCode -> JavaCode -> Assertion
assertEqAfterRRV before after = assertEqual (parseB after) (renameRedefinedVars (parseB before)) 
assertEqAfterRRVM before after = assertEqual (parseBMethod after) (renameRedefinedVars (parseBMethod before)) 

test_renameRedefinedVars_Basic :: Assertion
test_renameRedefinedVars_Basic = do
    assertEqAfterRRV   "x=0;x=1;x=2;" "x_1=0;x_2=1;x_3=2;"
    assertEqAfterRRV   "a=0;b=a;f(a, b);a=1;f(a);" "a_1=0;b_1=a_1;f(a_1, b_1);a_2=1;f(a_2);" 

test_renameRedefinedVars_Loop :: Assertion
test_renameRedefinedVars_Loop = do
    assertEqAfterRRV   "x=0;while(x) {x=1;x=2;} x=3;" "x_1=0;while(x_1) {x_1=1;x_1=2;} x_2=3;"
    -- rename x_1 in if-else?
    assertEqAfterRRV   "x=0;if(x) {x=1;} else { x=2;} x=3;" "x_1=0;if(x_1) {x_1=1;} else { x_1=2;} x_2=3;"

test_renameRedefinedVars_method :: Assertion
test_renameRedefinedVars_method = do
    assertEqAfterRRVM   "void f(int x){x=x+1;x=x+2;f(x);}" "void f(int x){x_1=x+1;x_2=x_1+2;f(x_2);}"

--------------------------------------------------------------------------------
-- Remove unused vars tests

assertEqAfterRUV :: JavaCode -> JavaCode -> Assertion
assertEqAfterRUV before after = assertEqual (parseB after) (removeUnusedVars (parseB before)) 

assertSameAfterRUV :: JavaCode -> Assertion
assertSameAfterRUV before = assertEqAfterRUV before before

test_remUnusedVarsBasic :: Assertion
test_remUnusedVarsBasic = do
    assertEqAfterRUV   "x=0;" ";" 
    assertSameAfterRUV "x=0;f(x);" 
    assertEqAfterRUV   "x=0;f(x);x=2;" "x=0;f(x);"
    assertEqAfterRUV   "x=1;x=2;f(x);" "x=2;f(x);" 
    assertSameAfterRUV  "a=0;b=1;x=a+b;f(x);" 
    assertSameAfterRUV  "x=0;x=x+5;f(x);" 
    assertEqAfterRUV    "a=2;y=a;f(x);" "f(x);" 

test_remUnusedVarsLoop :: Assertion
test_remUnusedVarsLoop = do
    assertEqAfterRUV   "while(1) x=5;" "while(1);"
    assertSameAfterRUV "while(1) {f(x); x=5;}"
    assertSameAfterRUV "x = 1;while(x) { }"
    assertSameAfterRUV "y = 3;while(x) { f(y); }"
    assertSameAfterRUV "while(x) { x++; }"
    assertEqAfterRUV "while(x) { x=1;x=2; }" "while(x) { x=2; }"

test_remUnusedVarsIf :: Assertion
test_remUnusedVarsIf = do
    assertEqAfterRUV   "if(1) { f(x); x=2;}" "if(1){ f(x);}" 
    assertEqAfterRUV   "x=1; if(1) { x=2;} else {x=3;} f(x);" "if(1) { x=2;} else {x=3;} f(x);" 
    assertSameAfterRUV "if(1) { f(x); x=2;} g(x);" 
    assertSameAfterRUV "x=1; if(1) { x=2;} else {} f(x);" 
    assertSameAfterRUV "x = 1;if(x) { } else {}"
    
test_remUnusedVarsNested :: Assertion
test_remUnusedVarsNested = do
   assertSameAfterRUV   "if(1) { while(x) { y = 1; } f(y);}"   
   assertEqAfterRUV   "while(x) { if(1) {x=1;x=2;} }" "while(x) { if(1) {x=2;} }"
   assertEqAfterRUV   "while(p) { if(1) x=1; else y=2; }" "while(p) { if(1) ;else;}"
   assertSameAfterRUV "while(p) { if(1) x=1; else y=2; f(x+y);}"

test_remUnusedVarsMethod :: Assertion
test_remUnusedVarsMethod = do
   assertEqual (parseBMethod "void x(int x) { f(x);}") (removeUnusedVars (parseBMethod "void x(int x) { f(x); a=0; }")) 

-- other function
test_remXXBreak :: Assertion
test_remXXBreak = do
   assertEqAfterRUV "x = 1; while(y) { break; x = 2; } f(x);" "x = 1; while(y) { break; } f(x);" 

--------------------------------------------------------------------------------
-- 

test_replaceContinue :: Assertion
test_replaceContinue = do
    assertEqAfterRC "while(p) {if(q) {continue;} f();}" "while(p) {if(q) {} else {f();}}"
    assertEqAfterRC "while(p) {if(p) {g(); } else continue; h();}" "while(p) {if(p) {g();h();} else {}}"
    assertEqAfterRC "while(p) {a(); if(p) {b();continue; } else c(); d();}" "while(p) {a();if(p) {b();} else {c();d();}}"

    assertNothingAfterRC "while(p) continue;"
    assertNothingAfterRC "while(p) if(p) if (q) continue;"
    assertNothingAfterRC "while(p) while (q) continue;"
    where
        assertEqAfterRC before after = assertEqual 
            (parseBS after) 
            (fromJust . removeContinue . parseBS $ before)
        assertNothingAfterRC = assertNothing . removeContinue . parseBS 


test_for2while :: Assertion
test_for2while = do
    assertEqAfterF2w "for (int i = 0; i < 10;i++) f();" "{int i = 0; while(i < 10) { f(); i++; }}"
    assertEqAfterF2w "for (int a=0, b=0; a < 10;) f();" "{int a=0, b=0; while(a < 10) f();}" 
    assertEqAfterF2w "for (c=1, d=1;c < 10;) f();" "{c=1; d=1; while(c < 10) f();}" 
    assertEqAfterF2w "for (;;);" "{while(true);}" 

    -- with continues
    assertEqAfterF2w "for (i = 0; i < 10;i++) if(x) continue;"
        "{i = 0; while(i < 10) { if(x) {i++; continue;} i++; }}"
    assertEqAfterF2w "for (i = 0; i < 10;i++) if(1) continue; else if(2) continue; else while(3) continue;"
        "{i = 0; while(i < 10) { if(1) {i++; continue;} else if(2) {i++; continue;} else while(3) continue; i++; }}"
    where
        assertEqAfterF2w before after = assertEqual 
            ( forceParseStat $ after) 
            (fromJust . for2while . forceParseStat $ before)
--------------------------------------------------------------------------------
-- 


test_SPV4sepInitDecl :: Assertion
test_SPV4sepInitDecl = do
    assertBool $ "int x; x=0;"  ~~~ "int x = 0;"
    assertBool $ "int x, y=1;"  ~~~ "int x; int y; y= 1;"
    assertBool $ "int x, y;"    ~~~ "int x;int y;"
    assertBool $ "int x, y;"    ~~~ "int y;int x;"
     
test_SPV3Braces :: Assertion
test_SPV3Braces = do
    assertBool $ parse "if(1) f();"             ~~ parse "if(1){ f(); }"
    assertBool $ parse "if(1) while(2) f();"    ~~ parse "if(1){ while(2) {f();} }"
    assertBool $ parse "if(1);"                 ~~ parse "if(1) {}" 
    
test_normaliseTime:: Assertion
test_normaliseTime = do
    let x = normaliseF $ parse largeProgram
    return ()

test_normaliseTime2:: Assertion
test_normaliseTime2 = do
    let x = normaliseF $ parse program2S
    return ()
        
test_equivalentPrograms = do
    assertBool $ parse "x=1+2;print(x);"   <== parse "x=2+1;print(x);"
    assertBool $ parse "if(true)print(1);" <== parse "print(1);"
  
        --equiv = foldl isEquivalent True []

test_equivalentPrefix = do
    assertBool $ parse "print(1);" <== parse "print(1);print(2);"
    assertBool $ not $ parse "print(2);" <== parse "print(1);print(2);"

test_isPredecessor:: Assertion
test_isPredecessor = do
     assertBool $ "x=?;" `assertIsPredecessor` "x=2+1;"
     assertBool $ "?+?;" `assertIsPredecessor` "2+1;"
     assertBool $ "x=2+?;" `assertIsPredecessor` "x=2+1;"
     assertBool $ "" `assertIsPredecessor` "1;2;3;"
     assertBool $ "1;" `assertIsPredecessor` "1;2;3;"
     assertBool $ "1;2;" `assertIsPredecessor` "1;2;3;"
     assertBool $ "if(1);" `assertIsPredecessor` "if(1)1;"
     assertBool $ "if(?);" `assertIsPredecessor` "if(1)1;"
     assertBool $ "while(1)if(1){}" `assertIsPredecessor` "while(1) if(1){1;2;3;}"
     assertBool $ "while(1);" `assertIsPredecessor` "while(1)1;"
     assertBool $ "?;" `assertIsPredecessor` "1;"
          
     assertBool $ not $ "1;" `assertIsPredecessor` ""
     assertBool $ not $ "x=1;" `assertIsPredecessor` "x=?;" 
     
     -- holes
     assertBool $ "x=1+?;" `assertIsPredecessor` "x=?;"
     assertBool $ "! ?;" `assertIsPredecessor` "!a && !b;"
     assertBool $ "f(?, ?+?);" `assertIsPredecessor` "f(1, 2);"
     
     assertBool $ not $ "f(?, ?+?);" `assertIsPredecessor` "g(1, 2);"
     assertBool $ not $ "1;" `assertIsPredecessor` "2;"
     assertBool $ not $ "a[?];p(a);" `assertIsPredecessor` "a[1];p(b);"
     
     assertBool $ "x=?;" `assertIsPredecessor` "x={1,2,3};"   
     assertBool $ "int x=?;" `assertIsPredecessor` "int x = 1;"
     assertBool $ "int [] x=?;" `assertIsPredecessor` "int [] x = {1,2,3};"
     assertBool $ "int [] ?;" `assertIsPredecessor` "int [] x = ?;"
     
     assertBool $ "?+(?*?);" `a` "a+(b*c);"
     assertBool $ "a+(?*?);" `a` "a+(b*c);"
     assertBool $ "(?*?)+?;" `a` "(b*c)+d;"
     assertBool $ "(?*?)+d;" `a` "(b*c)+d;"      
     
    where
        assertIsPredecessor = isPredecessor [] `on` parse
        a = assertIsPredecessor

test_loopAt0 :: Assertion
test_loopAt0 = do
   -- assertBool $ "for(i=2;i<10;i++) p(i);" ~~~ "for(i=0;(i+2)<10;(i+2)++) p(i+2);"
    --assertBool $ "for(i=2;i<10;i++) p(i);" ~~~ "for(i=0;(i+2)<10;i++) p(i+2);"
    -- assertBool $ "for(i=2;i<10;i++) p(i);" ~~~ "for(i=0;i<8;i++) p(i+2);"
    assertBool $ "for(int i=2;i<10;i++) p(i);" ~~~ "for(i=0;i<8;i++) p(i+2);"       

prop_normTime :: Program -> Bool
prop_normTime p = (/= toB p) $ fst $ normalisePartial p 
    

-- A program is a predecessor of itself (reflexive)
prop_predecessorOfSelf :: Program -> Bool
prop_predecessorOfSelf p = isPredecessor [] p p
       
program1S = "int sum1 = 0;;;\
\for (int i = 1; i < 100; i = i + 2)\
\   sum1 = sum1 + i;;;;\nprint(sum1);  ;"

program2S = " int sum2 = 0;\
\for (int i = 1; i < 100; i=i+1)\
\    if (i % 2 == 1)\
\        sum2 = sum2 + i;\
\print(sum2);"



