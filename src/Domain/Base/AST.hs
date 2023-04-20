{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
module Domain.Base.AST where

import Prelude hiding ((<$>))
import Data.Data hiding (DataType)
import Data.Maybe
import Data.Generics.Uniplate.DataOnly (universeBi)
import Text.PrettyPrint.Leijen
import Control.Monad

import Domain.Refactoring.Rules.Rules
-- import Domain.Transformation.ProgramTransformation (for2while)
import qualified Domain.Syntax as S
import Domain.Syntax (Holed, PExpr, DataType(..), Identifier(..), Literal(..), Fragment(..), 
    InfixOp(..), UnaryOp(..), Param(..), Modifier(..), getBlockStats, body )
import Utils.Utils

import Ideas.Common.Library




{-!
deriving instance UniplateDirect InfixOp
deriving instance UniplateDirect BProgram
deriving instance UniplateDirect BStat
deriving instance UniplateDirect BExpr
deriving instance UniplateDirect BForInit
!-}

-------------------------------------------------------------------------------    
-- | Data types for non-annotated programs

data BProgram = 
        Program 
        {
            bbody        :: [BStat]
        } 
    |   OOProgram [BClass]
    deriving (Data, Typeable, Eq, Show, Ord)  

data BClass = Class Identifier [BClassMember] deriving (Data, Typeable, Eq, Show, Ord)

data BClassMember =  
        Attribute [Modifier] DataType [BExpr] 
    |   BMethod 
        { 
            mReturnType :: Maybe DataType, 
            mId :: Identifier, 
            mParams :: [Param], 
            mBody :: BFragment,
            mMdfs :: [Modifier]
        }
    |   Constructor [Modifier] Identifier [Param] BFragment
    deriving (Data, Typeable,  Eq, Show, Ord)

data BFragment = BFragment { bstats :: [BStat] } deriving (Data, Typeable, Eq, Show, Ord)

data BStat = 
        Block      [BStat]
    |   IfElse     BExpr [BStat] [BStat]
    |   While      BExpr [BStat]
    |   Print      BExpr
    |   Assignment BExpr BExpr
    |   ExprStat   BExpr
    |   Break
    |   Continue  
    |   Return     (Maybe BExpr)
    deriving (Data, Typeable,  Eq, Show, Ord)

data BExpr =   
        Infixed     InfixOp BExpr BExpr
    |   Prefixed    UnaryOp BExpr
    |   Postfixed   UnaryOp BExpr
    |   LitExpr     Literal
    |   IdExpr      Identifier
    |   ArrayAcc    Identifier BExpr 
    |   Call        Identifier [BExpr] -- can be a ExprStat
    |   Property    Identifier Identifier
    |   NewArray    DataType BExpr
    |   Ternary     BExpr BExpr BExpr
    deriving (Data, Typeable, Eq, Ord, Show)

instance Num BExpr where
    fromInteger x   = S.makeInt (fromInteger x)
    x + y           = Infixed Addition x y
    x * y           = Infixed Multiplication x y
    x - y           = Infixed Subtraction x y
    negate          = Prefixed Minus
    abs             = undefined
    signum          = undefined
    
instance PExpr BExpr where
    lit0                = LitExpr S.iLit0
    lit1                = LitExpr S.iLit1
    trueLit             = LitExpr S.tLit
    falseLit            = LitExpr S.fLit
    emptyString         = LitExpr S.emptySLit
    isLit (LitExpr _)   = True
    isLit _             = False
    makeInt             = LitExpr . IntLiteral 
    makeIdt             = IdExpr . Identifier   
    isExprHole _        = False  

instance S.Inc BExpr where
    incExpr i (LitExpr (IntLiteral x)) = S.makeInt (x + i)
    incExpr i x = x + S.makeInt i 

{-
instance IsTerm BProgram where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm BStat where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm BFragment where
    toTerm   = toTermG
    fromTerm = fromTermG
-}
instance Different BExpr where
    different = (S.makeIdt "x", S.lit0)

instance Different BStat where
    different = (Break, Continue)
-------------------------------------------------------------------------------    
-- Util functions

-- | Find the single method and extract its code. 
-- Program should have one class
findMethod :: BProgram -> Maybe BClassMember
findMethod program = case program of
    OOProgram [Class _[m@BMethod {}]] -> Just m
    _                                 -> Nothing

 -- | Find the method with name and extract its code. 
findMethodByName :: Identifier -> BProgram -> Maybe BClassMember
findMethodByName mName p = let xs = findMethodsByName mName p
    in if null xs 
        then Nothing 
        else Just (head xs)
  
findMethodsByName :: Identifier -> BProgram -> [BClassMember]
findMethodsByName idt p = [m | m@BMethod {} <- universeBi p, mId m == idt ]

-- | Returns all methods in a OO program
findAllMethods :: BProgram -> [BClassMember]
findAllMethods p = [m | m@BMethod {} <- universeBi p]


mapBFragment :: ([BStat] -> [BStat]) -> BFragment -> BFragment
mapBFragment f = BFragment . f . bstats

isWhile :: BStat -> Bool
isWhile While {} = True
isWhile _        = False

isEmptyElse :: BStat -> Bool
isEmptyElse (IfElse _ _ []) = True
isEmptyElse  _              = False

isEmptyIf :: BStat -> Bool
isEmptyIf (IfElse _ [] _) = True
isEmptyIf  _              = False

isEmptyIfElse :: BStat -> Bool
isEmptyIfElse (IfElse _ [] []) = True
isEmptyIfElse  _               = False

idt :: BExpr -> Maybe Identifier
idt (IdExpr idt)     = Just idt
idt (ArrayAcc idt _) = Just idt
idt _                = Nothing 
--------------------------------------------------------------------------------------   
-- GENERATED START
-- GENERATED STOP

bprogramSymb, ooProgramSymb :: Symbol
bprogramSymb        = newSymbol "bprogram"
ooProgramSymb       = newSymbol "ooprogram"

instance IsTerm BProgram where
    toTerm (Program { bbody = p })  = TCon bprogramSymb (toTerm1 p)     
    toTerm (OOProgram cs)           = TCon ooProgramSymb [toTermList cs] 

    fromTerm t = case t of
        (TCon s [tbody]) | s == bprogramSymb -> do
            b <- fromTerm tbody
            return $ Program { bbody = b }
        (TCon s [cs]) | s == ooProgramSymb -> liftM OOProgram (fromTermList cs)   

        _ ->  fromTermError t

blockSymb, ifelseSymb, whileSymb, breakSymb,
    exprstatSymb, assignmentSymb, printSymb, returnSymb, continueSymb :: Symbol
blockSymb           = newSymbol "block"
ifelseSymb          = newSymbol "ifelse"
whileSymb           = newSymbol "whileSymb"
breakSymb           = newSymbol "break"
exprstatSymb        = newSymbol "exprstat"
assignmentSymb      = newSymbol "assignment"
printSymb           = newSymbol "print"
returnSymb          = newSymbol "return"
continueSymb        = newSymbol "continue"

instance IsTerm BStat where
    toTerm (Block xs)         = TCon blockSymb (map toTerm xs)
    toTerm (IfElse a b c)     = TCon ifelseSymb [toTerm a, toTermList b, toTermList c]
    toTerm (While a b)        = TCon whileSymb [toTerm a, toTermList b]
    toTerm (Print a)          = TCon printSymb (toTerm1 a)
    toTerm (Assignment a b)   = TCon assignmentSymb (toTerm2 a b)
    toTerm (ExprStat a)       = TCon exprstatSymb (toTerm1 a)
    toTerm Break              = TCon breakSymb []
    toTerm Continue           = TCon continueSymb []
    toTerm (Return a)         = TCon returnSymb (toTerm1 a)
    toTerm t                  = toTermError t

    fromTerm t = case t of -- check []
        (TCon s xs)           | s == blockSymb    -> liftM Block (mapM fromTerm xs)
        (TCon s [a, b, c])    | s == ifelseSymb   -> fromTerm3 IfElse a b c
        (TCon s [a, b])       | s == whileSymb    -> fromTerm2 While a b
        (TCon s [a])          | s == printSymb    -> fromTerm1 Print a
        (TCon s [a, b])       | s == assignmentSymb -> fromTerm2 Assignment a b
        (TCon s [a])          | s == exprstatSymb -> fromTerm1 ExprStat a
        (TCon s [])           | s == breakSymb    -> return Break
        (TCon s [])           | s == continueSymb -> return Continue
        (TCon s [a])          | s == returnSymb   -> fromTerm1 Return a
        _                                         -> fromTermError t

infixedSymb, prefixedSymb, postfixedSymb, literalexprSymb, idexprSymb,
    propertySymb, arrayaccSymb, callSymb, newarraySymb :: Symbol
infixedSymb         = newSymbol "infixed"
prefixedSymb        = newSymbol "prefixed"
postfixedSymb       = newSymbol "postfixed"
literalexprSymb     = newSymbol "literalexpr"
idexprSymb          = newSymbol "idexpr"
propertySymb        = newSymbol "property"
arrayaccSymb        = newSymbol "arrayacc"
callSymb            = newSymbol "call"
newarraySymb        = newSymbol "newarray"
ternarySymb         = newSymbol "ternary"

instance IsTerm BExpr where
    toTerm (Infixed a b c)    = TCon infixedSymb (toTerm3 a b c)
    toTerm (Prefixed a b)     = TCon prefixedSymb (toTerm2 a b)
    toTerm (Postfixed a b)    = TCon postfixedSymb (toTerm2 a b)
    toTerm (LitExpr a)        = TCon literalexprSymb (toTerm1 a)
    toTerm (IdExpr a)         = TCon idexprSymb (toTerm1 a)
    toTerm (Property a b)     = TCon propertySymb (toTerm2 a b)
    toTerm (ArrayAcc a b)     = TCon arrayaccSymb (toTerm2 a b)
    toTerm (Call a b)         = TCon callSymb [toTerm a, toTermList b] -- check
    toTerm (NewArray a b)     = TCon newarraySymb (toTerm2 a b)
    toTerm (Ternary a b c)    = TCon ternarySymb (toTerm3 a b c)
    toTerm t                  = toTermError t
     
    fromTerm t = case t of
        (TCon s [a, b, c])    | s == infixedSymb    -> fromTerm3 Infixed a b c
        (TCon s [a, b])       | s == prefixedSymb   -> fromTerm2 Prefixed a b
        (TCon s [a, b])       | s == postfixedSymb  -> fromTerm2 Postfixed a b
        (TCon s [a])          | s == literalexprSymb-> fromTerm1 LitExpr a
        (TCon s [a])          | s == idexprSymb     -> fromTerm1 IdExpr a
        (TCon s [a, b])       | s == propertySymb   -> fromTerm2 Property a b
        (TCon s [a, b])       | s == arrayaccSymb   -> fromTerm2 ArrayAcc a b
        (TCon s [a, b])       | s == callSymb       -> fromTerm2 Call a b
        (TCon s [a, b])       | s == newarraySymb   -> fromTerm2 NewArray a b
        (TCon s [a, b, c])    | s == ternarySymb    -> fromTerm3 Ternary a b c
        _                                           -> fromTermError t

instance IsTerm BFragment
instance IsTerm BClass
