{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Domain.TestPrograms where

import Domain.Syntax
import Test.QuickCheck
import Control.Monad
import Data.Sequence as Seq
import Text.PrettyPrint.Leijen (pretty, Pretty)

--------------------------------------------------------------------------------
-- Generating 'random' programs

data BlockId = BlockId { nr :: Int } deriving Show

instance Arbitrary BlockId where
    arbitrary = BlockId <$> choose (0, 3)
    
instance Arbitrary Program where
    arbitrary = do
        s <- vectorOf 5 arbitrary
        return $ makeEmptyProgram { body = makeBlock (s ++ [Print $ makeIdt "x"])} 

instance Arbitrary Statement where
    arbitrary = sized $ sizedStatGen True
     
sizedStatGen :: Bool -> Int -> Gen Statement
sizedStatGen holes n = oneof $ notNested ++ if n > 0 then nested else []
    where
        notNested = 
            [
                Print <$> arbEx,
                VarDeclarations IntType <$> sizedVector declExprGen (1, 3),
                return Break,
                return Continue,
                ExprStat <$> arbEx
            ]
        nested = 
            [ 
                If <$> arbEx <*> smallerStat,
                IfElse <$> arbEx <*> sizedBlock <*> smallerStat, -- avoid dangling else         
                While trueLit <$> smallerStat,        
                (For 
                    <$> ForInitExpr . (:[]) . Assignment Assign counter . makeInt <$> choose (0, 10)                   
                    <*> pure [Infixed Less counter $ makeInt 99]   
                    <*> pure [Postfixed Incr counter]
                    <*> smallerStat   
                )
            ]
        smallerStat = oneof [sizedStat, sizedBlock]
        counter     = makeIdt "i"
        arbEx       = exprGen holes
        sizedStat   = sizedStatGen holes $ n `div` 10
        sizedVector = (>=>) choose . flip vectorOf
        sizedBlock  = makeBlock <$> sizedVector sizedStat (1, 5)
          
instance Arbitrary Expression where
    arbitrary = exprGen True

exprGen :: Bool -> Gen Expression
exprGen holes = frequency $ 
        [   
            (40, makeInt <$> choose (0, 999)),-- no negative numbers, they are always parsed with operator
            (40, IdExpr <$> arbitrary),
            (50, arithExprGen),
            --(20, assignExprGen), 
            --(5, Postfixed Incr <$> arbEx),
            (5, Call <$> arbitrary <*> vectorOf 2 (exprGen holes)),
            (5, ArrayAcc <$> arbitrary <*> exprGen holes)
        ] -- ++ [ (10, HoleExpr <$> arbitrary) | holes ] 

                    
instance Arbitrary InfixOp where
    arbitrary = elements
        [   Less, LessOrEqual, Greater, 
            GreaterOrEqual, Multiplication, Subtraction,
            Addition, Division, Remainder, Equal,
            NotEqual, AND, OR
        ]

instance Arbitrary UnaryOp where
    arbitrary = elements [Minus, Plus, Not, Incr, Decr]

instance Arbitrary AssignOp where
    arbitrary = frequency $ 
        Prelude.zip [15, 4, 2, 2, 1] (map return [Assign, AssignAdd, AssignSub, AssignDiv, AssignRem]) 
      
   
instance Arbitrary Identifier where
    arbitrary = Identifier <$> elements ["x", "y", "z"]

instance Arbitrary Literal where
    arbitrary = oneof 
        [
            BoolLiteral <$> arbitrary,
            IntLiteral <$> arbitrary,
            StringLiteral <$> arbitrary,
            return Null,
            ArrayLit <$> (Seq.fromList <$> vectorOf 4 intGen)  -- arbitrary  
        ]

-- specific generators
intGen :: Gen Literal
intGen = IntLiteral <$> arbitrary 

randomPosInt :: Gen Expression
randomPosInt = makeInt <$> choose (0, 999) 

arithExprGen :: Gen Expression
arithExprGen = oneof 
    [
        Infixed <$> arbitrary <*> operand <*> operand,
        operand
    ]
    where
        operand = oneof [randomPosInt, IdExpr <$> arbitrary]

assignExprGen :: Gen Expression      
assignExprGen = Assignment <$> arbitrary <*> (IdExpr <$> arbitrary) <*> arithExprGen

declExprGen :: Gen Expression      
declExprGen = Assignment Assign <$> (IdExpr <$> arbitrary) <*> arithExprGen
     
smallBlockGen :: Gen Statement     
smallBlockGen = makeBlock <$> vectorOf 2 arbitrary

--------------------------------------------------------------------------------    
-- Generating programs that do not contain holes (such as model programs)

newtype CompleteProgram = CompleteProgram   { cprogram   :: Program } 
    
instance Show CompleteProgram where
    show = show . cprogram

instance Pretty CompleteProgram where
    pretty = undefined -- pretty . cprogram 
    
instance Arbitrary CompleteProgram where
    arbitrary = do
        stats <- vectorOf 3 $ sized $ sizedStatGen False
        return $ CompleteProgram $ makeEmptyProgram 
            { body = makeBlock (stats ++ [Print $ makeIdt "x"]) } 
        

        

