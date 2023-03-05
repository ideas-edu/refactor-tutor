{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances, FunctionalDependencies #-}
module Domain.Base.Conversion where

import Prelude hiding ((<$>))
import Data.Data hiding (DataType)
import Data.Maybe
import Data.Generics.Uniplate.DataOnly (universeBi)
import Text.PrettyPrint.Leijen

import Domain.Base.AST
import Domain.Refactoring.Rules.Rules
import Domain.Refactoring.Util
import Domain.Transformation.ProgramTransformation (for2while, foreachToFor)
import Domain.Terms

import Ideas.Common.Library
import qualified Domain.Syntax as S
import Domain.Syntax (Holed, PExpr, DataType(..), Identifier(..), Literal(..), Fragment(..), 
    InfixOp(..), UnaryOp(..), Param(..), Modifier(..), getBlockStats, body )
import Domain.Printer (prettyCommaList, holeSymbol, nested, indentSpaces, braced,
    prettyVList)


class ToBase a where
    toB :: a -> BProgram 
    
instance ToBase S.Program where
    toB p = case p of
        S.OOProgram {} -> conv p
        S.Program {}   -> (Program . concatMap conv . getBlockStats . body) p

instance ToBase BProgram where
    toB = id

baseView :: View S.Program BProgram
baseView = makeView (Just . toB) conv

-- Does not work properly
viewZ :: View (Context S.Program) (Context BProgram, Context S.Program)
viewZ = makeView f g
 where
  -- (Context S.Program -> Maybe (Context BProgram, S.P))
   f ctx = currentInContext ctx >>= \a -> Just (newTermContext (toB a), ctx)
   -- ((Context BProgram, S.P) -> Context S.Program
   g (cbp, cp) = case fromContext cbp of
    Just x  -> replaceInContext (conv x) cp
    Nothing -> cp

-------------------------------------------------------------------------------    
-- Conversion to base type
    
class Convert a b | a -> b where
    conv :: a -> b

instance (Convert a b) => Convert [a] [b] where
    conv = map conv

instance Convert S.Program BProgram where
    conv S.Program {body = (S.Block _ stats)} = Program (concatMap conv stats)
    conv (S.OOProgram classes)                = OOProgram (conv classes)

b2l :: S.Statement -> [BStat]
b2l (S.Block _ xs) = concatMap conv xs
b2l s = conv s

instance Convert S.Class BClass where
    conv (S.Class idt classMembers) = Class idt (conv classMembers)

instance Convert S.ClassMember BClassMember where
    conv (S.Attribute mdfs dt exps) = Attribute mdfs dt (conv exps)
    conv (S.Method mRetType idt params body mdfs) = BMethod
        {
            mReturnType = mRetType,
            mId = idt, 
            mParams = params, 
            mBody = conv body,
            mMdfs = mdfs
        }
    conv (S.Constructor mdfs idt params body) = Constructor mdfs idt params (conv body)

instance Convert S.Fragment BFragment where
    conv S.Fragment { stats = stats'} = BFragment (concatMap conv stats')

instance Convert S.Statement [BStat] where
    conv (S.Block _ xs)            = [Block (concatMap conv xs)]
    conv (S.If e s)                = [IfElse (conv e) (b2l s)  []      ]
    conv (S.IfElse e s1 s2)        = [IfElse (conv e) (b2l s1) (b2l s2)]
    conv (S.While e s)             = [While (conv e) (b2l s)]
    conv for@S.For {}              = conv (fromJust $ for2while for)
    conv fe@S.ForEach {}           = conv (fromJust $ foreachToFor fe)
    conv (S.Print e)               = [Print (conv e)]

    -- Types and declarations removed
    conv (S.VarDeclarations dt ex) = mapMaybe t ex
        where 
            -- keep declarations with value
            t a@(S.Assignment S.Assign l@(S.IdExpr _) r) = Just (Assignment (conv l) (conv r))
            t (S.IdExpr _)                               = Nothing
            t (S.HoleExpr _)                             = Nothing
            t (S.Assignment S.Assign (S.HoleExpr _) _)   = Nothing
            t s                                          = error $ "Invalid declaration " ++ show s

    conv (S.ArrayDecls dt (S.ArrDecl idt))   = []
    conv (S.ArrayDecls dt (S.ArrInit idt e)) = [Assignment (IdExpr idt) (conv e)]
    conv S.Empty                   = []

    -- Assignments to top level
    conv (S.ExprStat (S.Assignment S.Assign l r))           = [Assignment (conv l) (conv r)]
    conv (S.ExprStat e@(S.Postfixed S.Incr (S.IdExpr i)))   = maybe (error "incr to base") (conv . S.ExprStat) (apply replaceIncrement e)
    conv (S.ExprStat e@(S.Prefixed S.Incr (S.IdExpr i)))    = maybe (error "incr to base") (conv . S.ExprStat) (apply replaceIncrement e)
    conv (S.ExprStat e@(S.Postfixed S.Decr (S.IdExpr i)))   = maybe (error "decr to base") (conv . S.ExprStat) (apply replaceDecrement e)
    conv (S.ExprStat e@(S.Prefixed S.Decr (S.IdExpr i)))    = maybe (error "decr to base") (conv . S.ExprStat) (apply replaceDecrement e)
    conv (S.ExprStat e@(S.Assignment compound l r))         = maybe (error "compound to base") (conv . S.ExprStat) (apply (choice replaceCompoundOp) e)
    conv (S.ExprStat e)                                     = [ExprStat (conv e)]
    
    conv S.Break                   = [Break]
    conv S.Continue                = [Continue]
    conv (S.Return maybeE)         = [Return (fmap conv maybeE)]

    conv (S.Feedback _ s)          = conv s
    conv (S.MustUse s)             = conv s
    conv (S.Alt ss)                = conv $ head ss
    
    conv x                         = error ("Missing " ++ show x)
       
instance Convert S.Expression BExpr where
    conv (S.Infixed op e1 e2)      = Infixed op (conv e1) (conv e2)

    conv (S.Assignment _ _ _ )     = error "toB: Assignment in expr"

    conv (S.Prefixed op e)         = Prefixed op (conv e)
    conv (S.Postfixed op e)        = Postfixed op (conv e)
    conv (S.LiteralExpr l )        = LitExpr l
    conv (S.IdExpr i)              = IdExpr i
    conv (S.ArrayAcc i e)          = ArrayAcc i (conv e)
    conv (S.HoleExpr _)            = error "toB: Hole not supported"
    conv (S.Call i es)             = Call i (conv es)
    conv (S.Property i1 i2)        = Property i1 i2 
    conv (S.NewArray dt e)         = NewArray dt (conv e)

instance Convert S.ForInit [BExpr] where
    conv (S.ForInitExpr es)        = conv es
    conv (S.ForInitDecls dt es)    = conv es
    conv (S.FHole _)               = error "toB: Hole not supported"

-------------------------------------------------------------------------------    
-- Base to Program Conversion

l2b :: [BStat] -> S.Statement
l2b = S.Block 0 . conv

instance Convert BProgram S.Program where
    conv p = case p of
        Program {}     -> S.makeProgram (conv (bbody p)) 
        (OOProgram cs) -> S.makeOOProgram (conv cs)

instance Convert BClass S.Class where
    conv (Class idt cms) = S.makeClass idt (conv cms)

instance Convert BClassMember S.ClassMember where
    conv p = case p of
        BMethod {} -> S.Method
            { S.mReturnType = mReturnType p
            , S.mId         = mId p
            , S.mParams     = mParams p
            , S.mBody       = conv (mBody p)
            , S.mMdfs       = mMdfs p
            }
        Attribute mdfs dt exps    -> S.Attribute mdfs dt (conv exps)
        Constructor mdfs idt ps f -> S.Constructor mdfs idt ps (conv f)

instance Convert BFragment S.Fragment where
    conv f = S.Fragment { fLocId = 0, stats = conv (bstats f) }

-- Type declarations are lost
instance Convert BStat S.Statement where
    conv s = case s of             
        Print e               -> S.Print (conv e)   
        IfElse e s []         -> S.If (conv e) (l2b s)
        IfElse e s1 s2        -> S.IfElse (conv e) (l2b s1) (l2b s2)       
        While e s             -> S.While (conv e) (l2b s)                 
        Block xs              -> l2b xs      
        ExprStat e            -> S.ExprStat (conv e)  
        Break                 -> S.Break
        Continue              -> S.Continue
        Return me             -> S.Return (fmap conv me)
        Assignment l r        -> S.ExprStat (S.Assignment S.Assign (conv l) (conv r))

instance Convert BExpr S.Expression where
    conv expr = case expr of
        IdExpr i            -> S.IdExpr i
        LitExpr l           -> S.LiteralExpr l
        Infixed op e1 e2    -> S.Infixed op (conv e1) (conv e2)
        Prefixed op e       -> S.Prefixed op (conv e)
        Postfixed op e      -> S.Postfixed op (conv e)
        Call i ps           -> S.Call i (conv ps)
        ArrayAcc i index    -> S.ArrayAcc i (conv index)
        Property i prop     -> S.Property i prop
        NewArray dt e       -> S.NewArray dt (conv expr)   

-------------------------------------------------------------------------------
-- Prints a base program

instance Pretty BProgram where
    pretty = pretty . conv

instance Pretty BFragment where
    pretty = pretty . conv

instance Pretty BClass where
    pretty = pretty . conv

instance Pretty BClassMember where
    pretty = pretty . conv

instance Pretty BExpr where
    pretty = pretty . conv
        
instance Pretty BStat where  
    pretty = pretty . conv