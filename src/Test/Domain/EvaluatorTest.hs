{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Domain.EvaluatorTest where

import Domain.Evaluator
import Domain.Domain
import Domain.Syntax 
import Domain.Base.Conversion
import Domain.Base.AST as B
import Domain.Parsers.JavaParser
import Domain.Refactoring.HCExercises
import Utils.TestParser (parseTest)
import Exp
import Test.Framework hiding ((.&.))
import Test.Framework.TestInterface (Assertion)
import Test.TestUtils
import Data.Maybe
import Data.Either
import Data.Sequence
import Control.Arrow
import Control.Monad
import Data.Bits

evalC       = fmap evalProgram . parseFragment' 
evalCode    = evalProgram . fromJust . parseFragment' 
evaLMethod' = evalMethod . parseBCM 
--3102
evaltest1 = "y=0;for(i = 1; i < 100;i++) { a = 3;while (a>0 && i<55){ x = i*i+2;a--;} if (x>50) y += 2;} print(--x + --y);"

test_eval:: Assertion 
test_eval = do
    assertEqual (evalCode evaltest1) $ Right "3102"
    assertEqual (Right "True") $ evalCode "b1 = true; b2 = false; print((5<=10)&&b2 || !b1 || b1==true);"
    assertEqual (Right "*+**+***+****+*****+******+*******+********+*********+**********+") $ evalCode "for(i = 1; i<=10;i++){ for(j=1;j<=i;j++)print(\"*\");print(\"+\");}"
    return ()

test_evalError:: Assertion
test_evalError = do
    _ <- assertLeft $ evalCode "x = 1 + True;" 
    _ <- assertLeft $ evalCode "print(1);print(2+True);"
    _ <- assertLeft $ evalCode "print(max(1,2) + (2+True));"
    return ()

test_evalCond :: Assertion
test_evalCond = do
    assertEqual (Right "2") (evalCode "int i = 0; int [] a = new int[2]; a[0]=1;a[1]=2; while(i<a.length && a[i] != -1) i++;print(i);") 
    assertEqual (Right "1") (evalCode "int i = 0; int [] a = new int[2]; a[0]=1;a[1]=-1; while(i<a.length && a[i] != -1) i++;print(i);")  

---------- Bitwise -----------


test_bitwiseAnd :: Assertion
test_bitwiseAnd = do
  assertEqual (expected 1 2) (actual 1 2)
  assertEqual (expected 6 7) (actual 6 7)
  where
    expected, actual :: Int -> Int -> Int
    expected n m = n .&. m
    actual n m = read $ fromRight undefined (evalCode ("print(" ++ show n ++ "&" ++ show m ++");"))

---------- Ternary ----------

test_ternary :: Assertion
test_ternary = do
  assertEqual "1" $ tern "true" "1" "2"
  assertEqual "2" $ tern "false" "1" "2"
  where
    tern c t f = fromRight undefined $ evalCode ("print(" ++ c ++ "?" ++ t ++ ":" ++ f ++ ");")

---------- Arrays ----------

test_evalArrays:: Assertion
test_evalArrays = do
    assertEqual (Right "3") $ evalCode "int []x = new int [2];x[0]=1;x[1]=2;print(x[0]+x[1]);"
    assertEqual (Right "2") $ evalCode "int []x = new int [2];print(x.length);"
    assertEqual (Right "2") $ evalCode "int []x = new int [2];x[0]=1;p=x[0];x[1]=p;print(x[0]+x[1]);"

test_evalArraysInit:: Assertion
test_evalArraysInit = do
    assertEqual (Right "6") $ evalCode "int []x = {1,2,3};print(x[0]+x[1]+x[2]);"

-- EJavaParser error
test_evalArrayErrors:: Assertion
test_evalArrayErrors = do       
    assertLeft $ evalCode "int [] x = new int[2]; print(x[-1]);"
    assertLeft $ evalCode "int [] x = new int[2]; print(x[2]);"
    assertLeft $ evalCode "int x = 0; print(x[0]);"
    return ()
    
test_evalVarInit:: Assertion
test_evalVarInit = do    
    assertRight $ evalCode "int x; x = 0;"
    assertRight $ evalCode "int [] x; x = new int [3];"
    return ()

---------- Loops ----------

test_evalBreak :: Assertion
test_evalBreak = do
    assertRight (evalMethod (parseBCM methodH) $ toIL [8])
      >>= assertEqual (Just $ iLit 5)
    -- nested break
    assertRight (evalMethod (parseBCM methodJ) $ toIL [])
      >>= assertEqual (Just $ iLit 15)

test_evalContinue :: Assertion
test_evalContinue = do
    assertRight (evalMethod (parseBCM methodI) $ toIL [1])
      >>= assertEqual (Just $ iLit 4)
    
methodH, methodI, methodJ :: JavaCode
methodH = "public int h(int x) { while (x<10) { if (x==5) break; x--; } return x; }"
methodI = "public int i(int x) { s = 0; while (x<=10) { x++; if (x>5) continue; s++;  } return s; }"
methodJ = "public int i() { int i=0, s = 0; while (i<5) { j = 0; while (j<5) {if (i>2) break; s++; j++; } i++; } return s; }"
       
---------- Methods ----------

test_evalMethodPrint :: Assertion
test_evalMethodPrint =     
    assertRight (evalMethod (parseBCM methodF) $ toIL [1, 2])
      >>= assertNothing

test_evalMethodRet :: Assertion
test_evalMethodRet =     
    assertRight (evalMethod (parseBCM methodG) $ toIL [1, 2])
      >>= assertEqual (Just $ iLit 3)

test_evalMethodMP :: Assertion
test_evalMethodMP = do   
    p <- example1MPMethod
    assertRight (evalMethod (conv . fromJust . stripMethod $ p) [ArrayLit $ fromList $ map iLit [1,2,3,4,-5], tLit])
      >>= assertEqual (Just $ iLit 10)

test_evalTestMethodCorrect :: Assertion
test_evalTestMethodCorrect = do    
    assertEmpty $ lefts testResults
    mapM_ (assertBoolVerbose "failing test cases?") (rights testResults)
      where
        testResults = testMethodWith (parseBCM methodG) correctTestCases

test_evalTestMethodIncorrect :: Assertion
test_evalTestMethodIncorrect = do    
    assertEmpty $ lefts testResults
    mapM_ (assertEqual False) (rights testResults)
      where  
        testResults = testMethodWith (parseBCM methodG) incorrectTestCases


test_evalField :: Assertion
test_evalField = void $ assertLeft (evalMethod (B.Attribute [] IntType []) $ toIL [1, 2])

-- Test program ----------------------------------------------------------------

test_evalMP :: Assertion
test_evalMP = do    
    p <- example1MPMethod
    assertEmpty $ lefts (testResults p)
    mapM_ assertBool (rights $ testResults p)
      where
        testResults method = testMethodWith (conv . fromJust . stripMethod $ method) testCasesSumValues    

-- Syntax ---------------------------------------------------------------------

test_findDataTypes :: Assertion
test_findDataTypes = assertRight 
        (findDataTypes <$> parseFragment "boolean p=0;String y; int b,c=0; String s,t;int i=1, j=2;int [] x;")
          >>= (\a -> do
                        assertBool ((makeIdentifier "p", BoolType)   `elem` a)
                        assertBool ((makeIdentifier "y", StringType) `elem` a)
                        assertBool ((makeIdentifier "b", IntType)    `elem` a)
                        assertBool ((makeIdentifier "c", IntType)    `elem` a)
                        assertBool ((makeIdentifier "s", StringType) `elem` a)
                        assertBool ((makeIdentifier "t", StringType) `elem` a)
                        assertBool ((makeIdentifier "i", IntType)    `elem` a)
                        assertBool ((makeIdentifier "j", IntType)    `elem` a)
                        assertBool ((makeIdentifier "x", ArrayType IntType) `elem` a)
              )

-------------------------------------------------------------------------------    
-- Testcase parsing

test_parseTestCaseCorrect :: Assertion
test_parseTestCaseCorrect = do
    assertJust $ parseTest "{1,2,3}; true; \"x\" -> 42"
    assertJust $ parseTest "{1,2,3}; true -> 42 -> x"
    assertJust $ parseTest "{1,2,3};true;\"x\"->42"
    assertJust $ parseTest "{}; 3 -> {1,2}"
    assertJust $ parseTest "-1 -> -1.5"
    return ()

test_parseTestCaseVoid :: Assertion
test_parseTestCaseVoid = void $ assertJust $ parseTest "1 -> "

test_parseTestCaseNoParams :: Assertion
test_parseTestCaseNoParams = void $ assertJust $ parseTest "-> 1"

test_parseTestCaseIncorrect :: Assertion
test_parseTestCaseIncorrect = do
    assertNothing $ parseTest "{1,2,3}; true"
    assertNothing $ parseTest "1->2;3"
    assertNothing $ parseTest "x->3"
    return ()

-------------------------------------------------------------------------------    
-- Utils

methodF, methodG :: JavaCode
methodF = "public void f(int x, int y) { print (x + y); }"
methodG = "public int g(int x, int y) { if (x <=y ) return (x + y); else return (x - y); }"


correctTestCases, incorrectTestCases :: [TestCase]
correctTestCases = map (makeTC . toIntLits')
    [ ([1, 2], 3)
    , ([3, 3], 6)
    , ([5, 1], 4) ]
incorrectTestCases = map (makeTC . toIntLits')
    [ ([2, 2], 0)
    , ([99, 11], 110) ]

toIntLits' = toIL *** iLit
toIL = map iLit



