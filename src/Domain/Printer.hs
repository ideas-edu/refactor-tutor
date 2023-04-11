{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Printer 
(
    ppJavaProgram,
    pretty,
    showPretty,
    prettyCommaList, holeSymbol, nested, indentSpaces, braced, prettyVList
)
where

import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen
import Domain.Syntax
import Data.Maybe
import Data.Char
import Data.Foldable (toList)

-- | Prints a program in Java Syntax
ppJavaProgram :: (Pretty p, IsProgram p) => p -> String
ppJavaProgram = showPretty 
    
instance Pretty Program where               
    pretty Program { body = Block _ stats } = prettyVList stats
    pretty (OOProgram classes) = pretty classes
    pretty p = pretty $ body p
    
instance Pretty Class where
    pretty (Class idt members) = 
        text "class" <+> pretty idt <$> braced members

    prettyList = prettyVList

instance Pretty ClassMember where
    pretty cm = case cm of 
        Attribute mdfs dt exps  -> 
            pretty mdfs <+> pretty dt <+> pretty exps <> semi
        
        Method retType idt mParams mBody mdfs -> 
                pretty mdfs 
            <+> pretty retType <+> pretty idt 
            <>  parens (prettyCommaList mParams)
            <$> braced mBody
        
        Constructor mdfs idt params body -> 
                pretty mdfs <+> pretty idt 
            <>  parens (prettyCommaList params)
            <$> braced body

    prettyList = prettyVList

instance Pretty (Maybe DataType) where
    pretty Nothing = text "void" 
    pretty (Just x) = pretty x  

instance Pretty DataType where
    pretty BoolType     = text "boolean"
    pretty IntType      = text "int"
    pretty StringType   = text "String"
    pretty DoubleType   = text "double" 
    pretty (ArrayType dt) = pretty dt <+> text "[]" 

instance Pretty Literal where    
    pretty (IntLiteral i)       = int i
    pretty (BoolLiteral b)      = text $ map toLower (show b)
    pretty (StringLiteral s)    = dquotes $ text s
    pretty Null                 = text "null"
    pretty (ArrayLit a)         = braces $ prettyCommaList $ toList a
    pretty (DoubleLiteral d)    = double d

instance Pretty Identifier where
    pretty (Identifier i) = text i

instance Pretty InfixOp where
    pretty Addition       = text "+"
    pretty Subtraction    = text "-"
    pretty Multiplication = text "*"
    pretty Division       = text "/"
    pretty Remainder      = text "%"
    pretty Equal          = text "=="
    pretty NotEqual       = text "!="
    pretty Greater        = text ">"
    pretty GreaterOrEqual = text ">="
    pretty Less           = text "<"
    pretty LessOrEqual    = text "<="
    pretty AND            = text "&&"
    pretty OR             = text "||"
    pretty BAnd           = text "&"
    {-pretty = text . fromMaybe "??" . flip lookup table
        where
            table = [(Addition, "+"), (Subtraction, "-"), (Multiplication, "*"), 
                    (Division, "/"), (Remainder, "%"),(Equal, "=="), 
                    (NotEqual, "!="), (Greater, ">"), (GreaterOrEqual, ">="), 
                    (Less, "<"), (LessOrEqual, "<="), (AND, "&&"), (OR, "||")]-}

instance Pretty AssignOp where
    pretty = text . fromMaybe "?=" . flip lookup table
        where
            table = [(Assign, "="), (AssignAdd, "+="), (AssignSub, "-="), 
                    (AssignMul, "*="), (AssignDiv, "/="), (AssignRem, "%=")]
                    
instance Pretty UnaryOp where
    pretty op = text $ case op of 
        Plus    -> "+"
        Minus   -> "-"
        Not     -> "!"
        Incr    -> "++"
        Decr    -> "--"

instance Pretty Expression where
    pretty expr = case expr of
        IdExpr i            -> pretty i
        LiteralExpr l       -> pretty l
        Infixed op e1 e2    -> maybeParen op e1 <+> pretty op <+> maybeParen op e2
        -- Infixed op e1 e2    -> parenthesized e1 <+> pretty op <+> parenthesized e2
        Prefixed op e       -> pretty op <> maybeParen op e -- parenthesized e
        Postfixed op e      -> parenthesized e <> pretty op
        HoleExpr i          -> holeSymbol -- <+> text "/*" <+> text (show i) <+> text "*/"
        Assignment op e1 e2 -> pretty e1 <+> pretty op <+> pretty e2
        Call i ps           -> pretty i <> parens (prettyCommaList ps)
        ArrayAcc i index    -> pretty i <> brackets (pretty index)
        Property i prop     -> pretty i <> text "." <> pretty prop
        NewArray dt e       -> text "new" <+> pretty dt <+> brackets (pretty e) 
        where
            parenthesized = parenthesizedExpr pretty
            -- parens if needed
            maybeParen parOp e = maybe (pretty e) (\op -> f op parOp e) (getOpPrec e)
            f op parOp = if op > precedence parOp || op == 14 -- OR
                            then parens . pretty  
                            else pretty
    
    prettyList = prettyCommaList

instance Pretty Statement where  
    pretty stat = case stat of      
        VarDeclarations dt decls ->
                pretty dt  
            <+> prettyCommaList decls
            <>  semi
    
        ArrayDecls dt arrayDecl ->
                pretty dt 
            <+> pretty arrayDecl
            <>  semi

        For a b c s ->
                text "for" <+> parens (
                    pretty a <> semi <+> 
                    prettyCommaList b <> semi <+>
                    prettyCommaList c ) 
            <$> nested pretty s 
        
        ForEach t i e s -> 
                text "for" <+> parens (
                    pretty t <+> pretty i <+>
                    colon <+> pretty e )
            <$> nested pretty s 

        Print e -> text "print" <+> parens' e <> semi
        
        If e s -> 
                text "if" <+> parens' e 
            <$> nested pretty s   
            
        IfElse e s1 s2 ->
                text "if" <+> parens' e 
            <$> nested pretty s1
            <$> text "else"
            <$> nested pretty s2
            
        While e s -> 
                text "while" <+> parens' e 
            <$> nested pretty s    
            
        Block i xs -> braced xs
        
        ExprStat e     -> pretty e <> semi   
        
        Break          -> text "break" <> semi
        
        Continue       -> text "continue" <> semi
        
        Empty          -> semi

        Function retType fId fParams fBody ->
                (if isJust retType then pretty retType else text "void")
            <+> pretty fId 
            <+> parens (prettyCommaList fParams) 
            <$> braces (line <> indent indentSpaces (pretty fBody) <> line)
        
        Return Nothing -> text "return" <> semi
        Return (Just ex) -> text "return" <+> pretty ex <> semi

        _               -> text "/* no pp */"

    prettyList = prettyVList
 
instance Pretty Fragment where
    pretty (Fragment _ stats) = pretty stats

instance Pretty ForInit where
    pretty (ForInitExpr es)     = prettyCommaList es
    pretty (ForInitDecls dt es) = pretty dt <+> prettyCommaList es
    pretty (FHole nr)           = holeSymbol -- <+> text "/*" <+> text (show nr) <+> text "*/"

instance Pretty ArrayDecl where 
    pretty (ArrDecl identifier) = pretty identifier
    pretty (ArrInit identifier array) = pretty identifier <+> pretty Assign <+> pretty array
    
instance Pretty Param where
    pretty (Param dType identifier) = pretty dType <+> pretty identifier
    pretty (PHole _) = holeSymbol

    prettyList = hsep . map pretty

instance Pretty Modifier where
    pretty Public  = text "public"
    pretty Private = text "private"
    pretty Static  = text "static"
    pretty Final  = text "final"

    prettyList = hsep . map pretty


--------------------------------------------------------------------------------
-- Util

holeSymbol :: Doc
holeSymbol = text "?"

indentSpaces :: Int
indentSpaces = 4

indent' :: Doc -> Doc
indent' = indent indentSpaces

prettyCommaList :: Pretty a => [a] -> Doc                
prettyCommaList = hsep . punctuate comma . map pretty 

nested :: (Statement -> Doc) -> Statement -> Doc
nested toDoc s = case s of 
    (Block _ _) -> toDoc s
    _           -> indent indentSpaces $ toDoc s

prettyVList :: Pretty a => [a] -> Doc
prettyVList = vsep . map pretty

braced :: Pretty a => a -> Doc
braced d = braces (line <> indent' (pretty d) <> line)

parens' :: Pretty a => a -> Doc
parens' = parens . pretty

-- avoiding parens at top level
parenthesizedExpr :: (Expression -> Doc) -> Expression -> Doc
parenthesizedExpr toDoc (Infixed op e1 e2) =  ( --parens
            parenthesizedExpr toDoc e1 
        <+> pretty op 
        <+> parenthesizedExpr toDoc e2
    )
parenthesizedExpr toDoc (Assignment op l r) = parens (
            parenthesizedExpr toDoc l 
        <+> pretty op 
        <+> parenthesizedExpr toDoc r
    )
parenthesizedExpr toDoc e = toDoc e

showPretty :: Pretty a => a -> String             
showPretty = show . pretty        
