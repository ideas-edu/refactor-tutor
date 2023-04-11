module Domain.Terms where

import Domain.Syntax
import Utils.Utils
import Ideas.Common.Rewriting.Term

import Control.Monad
import qualified Data.Sequence as S
import Data.Foldable

programSymb, ooProgramSymb :: Symbol
programSymb         = newSymbol "program"
ooProgramSymb       = newSymbol "ooprogram"

blockSymb, ifSymb, ifelseSymb, forSymb, foreachSymb, whileSymb, emptySymb, breakSymb,
    exprstatSymb, vardeclarationsSymb, printSymb, returnSymb, continueSymb :: Symbol
blockSymb           = newSymbol "block"
ifSymb              = newSymbol "if"
ifelseSymb          = newSymbol "ifelse"
forSymb             = newSymbol "for"
foreachSymb         = newSymbol "foreach"
whileSymb           = newSymbol "whileSymb"
emptySymb           = newSymbol "empty"
breakSymb           = newSymbol "break"
exprstatSymb        = newSymbol "exprstat"
vardeclarationsSymb = newSymbol "vardeclarations"
printSymb           = newSymbol "print"
returnSymb          = newSymbol "return"
continueSymb        = newSymbol "continue"

infixedSymb, assignmentSymb, prefixedSymb, postfixedSymb, literalexprSymb, idexprSymb,
    propertySymb, arrayaccSymb, callSymb, newarraySymb :: Symbol
infixedSymb         = newSymbol "infixed"
assignmentSymb      = newSymbol "assignment"
prefixedSymb        = newSymbol "prefixed"
postfixedSymb       = newSymbol "postfixed"
literalexprSymb     = newSymbol "literalexpr"
idexprSymb          = newSymbol "idexpr"
propertySymb        = newSymbol "property"
arrayaccSymb        = newSymbol "arrayacc"
callSymb            = newSymbol "call"
newarraySymb        = newSymbol "newarray"

forinitexprSymb, forinitdeclsSymb :: Symbol
forinitexprSymb     = newSymbol "forinitexpr"
forinitdeclsSymb    = newSymbol "forinitdecls"

additionSymb, subtractionSymb, multiplicationSymb, divisionSymb, remainderSymb, equalSymb,
    notequalSymb, greaterSymb, greaterorequalSymb, lessSymb, lessorequalSymb, andSymb, orSymb, bAndSymb :: Symbol
additionSymb        = newSymbol "addition"
subtractionSymb     = newSymbol "subtraction"
multiplicationSymb  = newSymbol "multiplication"
divisionSymb        = newSymbol "division"
remainderSymb       = newSymbol "remainder"
equalSymb           = newSymbol "equal"
notequalSymb        = newSymbol "notequal"
greaterSymb         = newSymbol "greater"
greaterorequalSymb  = newSymbol "greaterorequal"
lessSymb            = newSymbol "less"
lessorequalSymb     = newSymbol "lessorequal"
andSymb             = newSymbol "and"
orSymb              = newSymbol "or"
bAndSymb            = newSymbol "band"

assignSymb, assignremSymb, assignsubSymb, assigndivSymb, assignmulSymb,
    assignaddSymb :: Symbol
assignSymb          = newSymbol "assign"
assignmulSymb       = newSymbol "assignmul"
assignaddSymb       = newSymbol "assignadd"
assignsubSymb       = newSymbol "assignsub"
assigndivSymb       = newSymbol "assigndiv"
assignremSymb       = newSymbol "assignrem"

plusSymb, minusSymb, notSymb, incrSymb, decrSymb :: Symbol
plusSymb            = newSymbol "plus"
minusSymb           = newSymbol "minus"
notSymb             = newSymbol "not"
incrSymb            = newSymbol "incr"
decrSymb            = newSymbol "decr"

intliteralSymb, boolliteralSymb, doubleliteralSymb, stringliteralSymb,
    nullSymb, arrayliteralSymb :: Symbol
intliteralSymb      = newSymbol "intliteral"
boolliteralSymb     = newSymbol "boolliteral"
doubleliteralSymb   = newSymbol "doubleliteral"
stringliteralSymb   = newSymbol "stringliteral"
nullSymb            = newSymbol "null"
arrayliteralSymb    = newSymbol "arraylit"

booltypeSymb, inttypeSymb, doubletypeSymb, stringtypeSymb, arraytypeSymb :: Symbol
booltypeSymb        = newSymbol "booltype"
inttypeSymb         = newSymbol "inttype"
doubletypeSymb      = newSymbol "doubletype"
stringtypeSymb      = newSymbol "stringtype"
arraytypeSymb       = newSymbol "arraytype"

identifierSymb :: Symbol
identifierSymb      = newSymbol "identifier"

methodSymb, classSymb, publicSymb, staticSymb, finalSymb, paramSymb, 
    privateSymb, fragmentSymb :: Symbol
methodSymb          = newSymbol "method"
classSymb           = newSymbol "class"
publicSymb          = newSymbol "public"
privateSymb         = newSymbol "private"
staticSymb          = newSymbol "static"
finalSymb           = newSymbol "final"
paramSymb           = newSymbol "param"
fragmentSymb        = newSymbol "fragment"

instance IsTerm Program where
    toTerm (Program { body = p })   = TCon programSymb (toTerm1 p)     
    toTerm (OOProgram cs)           = TCon ooProgramSymb [toTermList cs] 

    fromTerm t = case t of
        (TCon s [tbody]) | s == programSymb -> do
            b <- fromTerm tbody
            return $ makeEmptyProgram { body = b}
        (TCon s [cs]) | s == ooProgramSymb -> liftM OOProgram (fromTermList cs)   

        _ ->  fromTermError t

-- incomplete 
instance IsTerm Statement where
    toTerm (Block _ xs)       = TCon blockSymb (map toTerm xs)
    toTerm (If a b)           = TCon ifSymb (toTerm2 a b)
    toTerm (IfElse a b c)     = TCon ifelseSymb (toTerm3 a b c)
    toTerm (While a b)        = TCon whileSymb (toTerm2 a b)
    toTerm (For a b c d)      = TCon forSymb (toTerm4 a b c d)
    toTerm (ForEach a b c d)  = TCon foreachSymb (toTerm4 a b c d)
    toTerm (Print a)          = TCon printSymb (toTerm1 a)
    toTerm (VarDeclarations a xs) = TCon vardeclarationsSymb (toTerm2 a xs)
    --
    toTerm Empty              = TCon emptySymb []
    toTerm (ExprStat a)       = TCon exprstatSymb (toTerm1 a)
    toTerm Break              = TCon breakSymb []
    toTerm Continue           = TCon continueSymb []
    --
    toTerm (Return a)         = TCon returnSymb (toTerm1 a)
    toTerm t                  = toTermError t

    fromTerm t = case t of
        (TCon s xs)           | s == blockSymb    -> liftM makeBlock (mapM fromTerm xs)
        (TCon s [a, b])       | s == ifSymb       -> fromTerm2 If a b
        (TCon s [a, b, c])    | s == ifelseSymb   -> fromTerm3 IfElse a b c
        (TCon s [a, b])       | s == whileSymb    -> fromTerm2 While a b
        (TCon s [a, b, c, d]) | s == forSymb      -> fromTerm4 For a b c d
        (TCon s [a, b, c, d]) | s == foreachSymb  -> fromTerm4 ForEach a b c d
        (TCon s [a])          | s == printSymb    -> fromTerm1 Print a
        (TCon s [a, b])       | s == vardeclarationsSymb -> fromTerm2 VarDeclarations a b
        --
        (TCon s [])           | s == emptySymb    -> return Empty
        (TCon s [a])          | s == exprstatSymb -> fromTerm1 ExprStat a
        (TCon s [])           | s == breakSymb    -> return Break
        (TCon s [])           | s == continueSymb -> return Continue
        
        (TCon s [a])          | s == returnSymb   -> fromTerm1 Return a

        (TMeta i)                                 -> return $ ExprStat (IdExpr (makeIdentifier $ "[stat" ++ show i ++ "]"))
        _                                         -> fromTermError t

-- incomplete 
instance IsTerm Expression where
    toTerm (Infixed a b c)    = TCon infixedSymb (toTerm3 a b c)
    toTerm (Assignment a b c) = TCon assignmentSymb (toTerm3 a b c)
    toTerm (Prefixed a b)     = TCon prefixedSymb (toTerm2 a b)
    toTerm (Postfixed a b)    = TCon postfixedSymb (toTerm2 a b)
    toTerm (LiteralExpr a)    = TCon literalexprSymb (toTerm1 a)
    toTerm (IdExpr a)         = TCon idexprSymb (toTerm1 a)
    toTerm (Property a b)     = TCon propertySymb (toTerm2 a b)
    toTerm (ArrayAcc a b)     = TCon arrayaccSymb (toTerm2 a b)
    toTerm (Call a b)         = TCon callSymb (toTerm2 a b)
    toTerm (NewArray a b)     = TCon newarraySymb (toTerm2 a b)
    toTerm t                  = toTermError t
     
    fromTerm t = case t of
        (TCon s [a, b, c])    | s == infixedSymb    -> fromTerm3 Infixed a b c
        (TCon s [a, b, c])    | s == assignmentSymb -> fromTerm3 Assignment a b c
        (TCon s [a, b])       | s == prefixedSymb   -> fromTerm2 Prefixed a b
        (TCon s [a, b])       | s == postfixedSymb  -> fromTerm2 Postfixed a b
        (TCon s [a])          | s == literalexprSymb-> fromTerm1 LiteralExpr a
        (TCon s [a])          | s == idexprSymb     -> fromTerm1 IdExpr a
        (TCon s [a, b])       | s == propertySymb   -> fromTerm2 Property a b
        (TCon s [a, b])       | s == arrayaccSymb   -> fromTerm2 ArrayAcc a b
        (TCon s [a, b])       | s == callSymb       -> fromTerm2 Call a b
        (TCon s [a, b])       | s == newarraySymb   -> fromTerm2 NewArray a b
        (TMeta i)                                   -> return (IdExpr (makeIdentifier "[expr]"))
        _                                           -> fromTermError t

instance IsTerm ForInit where
    toTerm (ForInitExpr xs)     = TCon forinitexprSymb (map toTerm xs)
    toTerm (ForInitDecls a xs)  = TCon forinitdeclsSymb [toTerm a, toTermList xs]
    toTerm t@(FHole _ )         = toTermError t

    fromTerm t = case t of
        (TCon s xs)       | s == forinitexprSymb  -> liftM ForInitExpr (mapM fromTerm xs)
        (TCon s [a, xs])  | s == forinitdeclsSymb -> fromTerm2 ForInitDecls a xs
        _ -> fromTermError t

instance IsTerm InfixOp where
    toTerm op = case op of
        Addition        -> TCon additionSymb []
        Subtraction     -> TCon subtractionSymb []
        Multiplication  -> TCon multiplicationSymb []
        Division        -> TCon divisionSymb []
        Remainder       -> TCon remainderSymb []
        Equal           -> TCon equalSymb []
        NotEqual        -> TCon notequalSymb []
        Greater         -> TCon greaterSymb []
        GreaterOrEqual  -> TCon greaterorequalSymb []
        Less            -> TCon lessSymb []
        LessOrEqual     -> TCon lessorequalSymb []
        AND             -> TCon andSymb []
        OR              -> TCon orSymb []
        BAnd            -> TCon bAndSymb []

    fromTerm t@(TCon s []) 
        | s == additionSymb       = return Addition
        | s == subtractionSymb    = return Subtraction
        | s == multiplicationSymb = return Multiplication
        | s == divisionSymb       = return Division
        | s == remainderSymb      = return Remainder
        | s == equalSymb          = return Equal
        | s == notequalSymb       = return NotEqual
        | s == greaterSymb        = return Greater
        | s == greaterorequalSymb = return GreaterOrEqual
        | s == lessSymb           = return Less
        | s == lessorequalSymb    = return LessOrEqual
        | s == andSymb            = return AND
        | s == orSymb             = return OR
        | s == bAndSymb           = return BAnd
        | otherwise               = fromTermError t
    fromTerm t = fromTermError t

instance IsTerm AssignOp where
    toTerm op = case op of
        Assign      -> TCon assignSymb []
        AssignMul   -> TCon assignmulSymb []
        AssignAdd   -> TCon assignaddSymb []
        AssignSub   -> TCon assignsubSymb []
        AssignDiv   -> TCon assigndivSymb []
        AssignRem   -> TCon assignremSymb []

    fromTerm t@(TCon s []) 
        | s == assignSymb    = return Assign
        | s == assignaddSymb = return AssignAdd
        | s == assignmulSymb = return AssignMul
        | s == assignsubSymb = return AssignSub
        | s == assigndivSymb = return AssignDiv
        | s == assignremSymb = return AssignRem
        | otherwise = fromTermError t
    fromTerm t = fromTermError t

instance IsTerm UnaryOp where
    toTerm op = case op of 
        Plus    -> TCon plusSymb []
        Minus   -> TCon minusSymb []
        Not     -> TCon notSymb []
        Incr    -> TCon incrSymb []
        Decr    -> TCon decrSymb []

    fromTerm t@(TCon s []) 
        | s == plusSymb  = return Plus
        | s == minusSymb = return Minus
        | s == notSymb   = return Not
        | s == incrSymb  = return Incr
        | s == decrSymb  = return Decr
        | otherwise      = fromTermError t
    fromTerm t = fromTermError t

instance IsTerm Literal where
    toTerm (IntLiteral i)     = TCon intliteralSymb [toTerm i]
    toTerm (BoolLiteral b)    = TCon boolliteralSymb [toTerm b]
    toTerm (DoubleLiteral d)  = TCon doubleliteralSymb [toTerm d]
    toTerm (StringLiteral s)  = TCon stringliteralSymb [toTerm s]
    toTerm (Null)             = TCon nullSymb []
    toTerm (ArrayLit a)       = TCon arrayliteralSymb [toTerm $ toList a]

    fromTerm t = case t of
        (TCon s [a])  | s == intliteralSymb     -> fromTerm1 IntLiteral a
                      | s == doubleliteralSymb  -> fromTerm1 DoubleLiteral a
                      | s == boolliteralSymb    -> fromTerm1 BoolLiteral a
                      | s == stringliteralSymb  -> fromTerm1 StringLiteral a
        (TCon s [])   | s == nullSymb           -> return Null
        (TCon s [a])  | s == arrayliteralSymb   -> liftM (ArrayLit . S.fromList) (fromTermList a)
        _ -> fromTermError t

instance IsTerm Identifier where
    toTerm i = TCon identifierSymb [toTerm $ name i]

    fromTerm (TCon s [TVar iname]) | s == identifierSymb = return Identifier { name = iname }
    fromTerm (TMeta _) = return Identifier { name = "[meta]" }

instance IsTerm DataType where
    toTerm dt = case dt of 
        BoolType       -> TCon booltypeSymb []
        IntType        -> TCon inttypeSymb []
        DoubleType     -> TCon doubletypeSymb []
        StringType     -> TCon stringtypeSymb []
        ArrayType at   -> TCon arraytypeSymb [toTerm at]

    fromTerm t = case t of
        (TCon s [])  | s == booltypeSymb    -> return BoolType
                     | s == inttypeSymb     -> return IntType
                     | s == doubletypeSymb  -> return DoubleType
                     | s == stringtypeSymb  -> return StringType
        (TCon s [a]) | s == arraytypeSymb   -> fromTerm1 ArrayType a
        _                                   -> fromTermError t

instance IsTerm Class where
    toTerm (Class a b) = TCon classSymb (toTerm2 a b)

    fromTerm t = case t of
        (TCon s [a, xs])  | s == classSymb -> fromTerm2 Class a xs
        _ -> fromTermError t

instance IsTerm ClassMember where
    toTerm (Method a b c d e) = TCon methodSymb (toTerm5 a b c d e)

    fromTerm t = case t of
        (TCon s [a,b,c,d,e])  | s == methodSymb -> fromTerm5 Method a b c d e
        _ -> fromTermError t

instance IsTerm Modifier where
    toTerm mdf = case mdf of 
        Public     -> TCon publicSymb []
        Private    -> TCon privateSymb []
        Static     -> TCon staticSymb []
        Final      -> TCon finalSymb []

    fromTerm t@(TCon s []) 
        | s == publicSymb  = return Public
        | s == privateSymb = return Private
        | s == staticSymb  = return Static
        | s == finalSymb   = return Final
        | otherwise        = fromTermError t
    fromTerm t = fromTermError t

instance IsTerm Param where
    toTerm t = case t of 
        (Param a b)   -> TCon paramSymb (toTerm2 a b)

    fromTerm t@(TCon s [a, b]) 
        | s == paramSymb   = fromTerm2 Param a b
        | otherwise        = fromTermError t
    fromTerm t = fromTermError t

instance IsTerm Fragment where
    toTerm (Fragment { fLocId = a, stats = b}) = TCon fragmentSymb (toTerm2 a b)

    fromTerm t = case t of
        (TCon s [a,b]) | s == fragmentSymb -> fromTerm2 (\a b -> Fragment { fLocId = a, stats = b}) a b
        _ -> fromTermError t
