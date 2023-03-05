{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.TestUtils where

import Test.Framework.TestInterface (Assertion)
import Test.Framework
import Data.Maybe (fromJust)
import Ideas.Service.State
import Ideas.Service.Diagnose
import FPTutor.Diagnose
import Ideas.Common.Library
import Control.Monad
import Domain.Parsers.JavaParser
import Domain.Syntax
import Domain.Base.AST
import Domain.Base.Conversion
import Domain.Refactoring.Util

-- Parsing --------------------------------------------------------------------

-- Assumes parsable input
parseB :: JavaCode -> BProgram
parseB = toB . forceParseFragment

-- | get the first statement  
parseBS :: JavaCode -> BStat
parseBS = head . bbody . parseB

parseBMethod :: JavaCode -> BProgram
parseBMethod = toB . forceParseMethod

parseBFragment :: JavaCode -> BFragment
parseBFragment = BFragment . bbody . parseB 

testParser = parseFragment'

-- force to return a classmember from code
parseBCM :: JavaCode -> BClassMember
parseBCM = conv . fromJust . stripMethod . forceParseMethod

parseEx = undefined

-- Asserting ------------------------------------------------------------------

-- uses old parser
assertEqInCtxt :: Apply a => a (Context Expression) -> JavaCode -> JavaCode -> IO ()
assertEqInCtxt strat input output = 
    assertJustEqC (parseEx output)
      (applyInNewContext strat $ parseEx input) 

assertEqBPInCtxt :: Apply a => a (Context BProgram) -> JavaCode -> JavaCode -> IO ()
assertEqBPInCtxt a input output = 
    assertJustEqC (parseB output) (applyInNewContext a . parseB $ input) 

assertEqBFInCtxt :: Apply a => a (Context BFragment) -> JavaCode -> JavaCode -> IO ()
assertEqBFInCtxt a input output = 
    assertJustEqC (parseBFragment output) (applyInNewContext a $ parseBFragment input) 

assertJustEqC :: (Show a, Eq a) => a -> Maybe (Context a) -> IO ()
assertJustEqC exp actual = assertJust actual >>= fromContext >>= assertEqual exp

assertJustEq :: (Show a, Eq a) => a -> Maybe a -> IO ()
assertJustEq exp actual = assertJust actual >>= assertEqual exp

-- Diagnosing -----------------------------------------------------------------


getType :: Diagnosis a -> String
getType Buggy {}            = "Buggy"
getType NotEquivalent {}    = "NotEquivalent"
getType Similar {}          = "Similar"
getType WrongRule {}        = "WrongRule"
getType Expected {}         = "Expected"
getType Detour {}           = "Detour"
getType Correct {}          = "Correct"
getType Unknown {}          = "Unknown"  
    
isExpected, isNotEquivalent, isCorrect :: Diagnosis a -> Bool                       
isExpected Expected {}              = True
isExpected _                        = False                      
isNotEquivalent NotEquivalent {}    = True
isNotEquivalent _                   = False                      
isCorrect Correct {}                = True
isCorrect _                         = False


-- Copied from Ideas.Service.Diagnose, commented out
-- also used in testutils
newState' :: Diagnosis a -> Maybe (State a)
newState' diagnosis =
   case diagnosis of
      Buggy _ _        -> Nothing
      NotEquivalent _  -> Nothing
      Similar  _ s     -> Just s
      WrongRule _ s _  -> Just s
      Expected _ s _   -> Just s
      Detour   _ s _ _ -> Just s
      Correct  _ s     -> Just s
      Unknown  _ s     -> Just s


largeProgram :: JavaCode
largeProgram = "int [] a ={1,3,2,8,4,5,0,6};    \
            \ int i,j,x;                                \
            \ for (i = 1 ; i < a.length ; i++){         \
            \ x = a[i];                                 \
            \ while ((i - 1 >= 0) && (x < a[i - 1]))    \
            \ {                                         \    
            \    a[i] = a[i - 1];                       \
            \    i--;                                   \
            \}                                          \
            \a[i] = x;                                  \
            \}                                          \
            \for (i = 1 ; i < a.length ; i++)print(a[i]);"