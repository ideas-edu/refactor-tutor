{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.Parsers.JavaParser 
(
    JavaCode,
    parseClass, parseFragment, parseMethod, parseWithTemplate, 
    parseClass', parseFragment', parseMethod', -- parsing to internal AST
    forceParseClass, forceParseMethod, forceParseFragment, forceParseStat,
    parseStat, -- TODO legacy, remove
    parseEJava, -- parsing to external AST
    currentTemplate,
    insertCodeInClassWith,
    parseStat1, parseExpr
    , CodeTemplate(..)
) where

import Prelude hiding (exp)
import Data.List hiding (init)
import Language.Java.Parser (parser, compilationUnit)
import Language.Java.Syntax as J 
import Language.Java.Pretty (prettyPrint)
import Text.Parsec.Error
import Utils.Either
import Data.Either
import Data.Maybe
import Domain.Syntax as S hiding (body, params, name)
import Utils.Utils (eitherToMaybe)
import Control.Monad
import qualified Data.Sequence as Seq

type JavaCode = String

-- TODO: check?
parseStat :: JavaCode -> Statement                              
parseStat = forceParseStat

-- | Parses the first statement found
parseStat1 :: JavaCode -> Maybe Statement                              
parseStat1 c = case parseFragment c of
    Left _   -> Nothing
    Right c' -> statAt 0 c'

parseExpr :: JavaCode -> Maybe Expression
parseExpr c = parseStat1 (insertCodeInTemplate exprTemplate c) >>= toExpr
    where
        toExpr :: Statement -> Maybe Expression
        toExpr (ExprStat (Assignment _ _ e)) = Just e
        toExpr _            = Nothing        

        exprTemplate = CodeTemplate { pre  = "tmp=", post = ";" }

--------------------------------------------------------------------------------
-- Functions for parsing to internal AST 

-- conv Parser type for exercises
parseClass, parseMethod, parseFragment :: JavaCode -> Either String Program
parseClass      = njpWithTemplate noTemplate
parseMethod     = njpWithTemplate classTemplate 
parseFragment   = join . fmap tryStrip . njpWithTemplate methodTemplate
    where tryStrip = maybeToEither "Main not found" . stripMainCode

parseWithTemplate :: CodeTemplate -> JavaCode -> Either String Program
parseWithTemplate = njpWithTemplate 

parseClass', parseFragment', parseMethod' :: JavaCode -> Maybe Program
parseClass'    = eitherToMaybe . parseClass
parseFragment' = eitherToMaybe . parseFragment
parseMethod'   = eitherToMaybe . parseMethod

forceParseFragment, forceParseMethod, forceParseClass :: JavaCode -> Program
forceParseFragment  = forceEither . parseFragment
forceParseMethod    = forceEither . parseMethod
forceParseClass     = forceEither . parseClass

-- | Returns the first statement found
forceParseStat :: JavaCode -> Statement
forceParseStat = fromJust . statAt 0 . forceParseFragment

njpWithTemplate :: CodeTemplate -> JavaCode -> Either String Program
njpWithTemplate template input = case parseEJava input template of
    Left err    -> Left $ "Parse error: " ++ show err
    Right res   -> cuToProgram res

newJavaParserTry :: JavaCode -> Either String Program
newJavaParserTry = either (Left . show) cuToProgram . asUnit 
    where
        --asMethod = flip parseEJava methodTemplate
        --asClass  = flip parseEJava classTemplate  
        asUnit   = flip parseEJava noTemplate   

--------------------------------------------------------------------------------
-- Functions for parsing to external AST

data CodeTemplate = CodeTemplate { pre :: String, post :: String}

-- Only use one template in all cases
currentTemplate, classTemplate, methodTemplate, noTemplate :: CodeTemplate
currentTemplate = noTemplate

classTemplate = CodeTemplate { 
    pre  = "public class X { ", 
    post = "}" 
}
methodTemplate = CodeTemplate { 
    pre  = "public class X { public static void main(String [] a) {",
    post = "}}" 
}
noTemplate = CodeTemplate { 
    pre  = "",
    post = "" 
}

insertCodeInClassWith :: String -> JavaCode -> JavaCode
insertCodeInClassWith className = insertCodeInTemplate (CodeTemplate { pre = "public class " ++ className ++ " { " , post = "}" } ) 

insertCodeInClass :: JavaCode -> JavaCode
insertCodeInClass = insertCodeInTemplate classTemplate 

insertCodeInTemplate :: CodeTemplate -> JavaCode -> JavaCode
insertCodeInTemplate template code = pre template ++ code ++ post template

parseEJava :: JavaCode -> CodeTemplate -> Either ParseError CompilationUnit
parseEJava code template = parser compilationUnit (insertCodeInTemplate template code)

forceParse :: (String -> Either ParseError a) -> String -> a
forceParse = (forceEither .)

parseEJavaPretty :: String -> CodeTemplate -> Either ParseError String
parseEJavaPretty code template = case parseEJava code template of 
    Left pErr -> Left pErr
    Right c   -> Right (prettyPrint c)

--------------------------------------------------------------------------------
-- Conversion to internal AST

cuToProgram :: Conv a b => a -> Either String b
cuToProgram parsed = case unCR (convert parsed) of
    Left err -> Left (unlines err)
    Right p  -> Right p

newtype ConvResult b = CR {unCR :: Either [String] b} 
data Converter' a b  = Conv {unConv :: a -> ConvResult b} 

instance Functor ConvResult where
    -- (a -> b) -> f a -> f b
    fmap f = CR . fmap f . unCR

instance Monad ConvResult where
    -- (>>=) :: forall a b. m a -> (a -> m b) -> m b 
    (>>=) x f   = CR $ (>>=) (unCR x) (unCR . f)
    return      = CR . Right
    fail msg    = CR $ Left [msg]
    
instance Applicative ConvResult where
    pure    = return
    (CR (Left xs)) <*> (CR (Left ys)) = CR (Left $ xs++ys) 
    a <*> b = CR $ unCR a <*> unCR b 
    
class Conv a b where -- | a -> b where
    converter   :: Converter' a b -- arrow?
    convert     :: a -> ConvResult b
    mapConvert  :: [a] -> ConvResult [b]

    mapConvert = mapM convert
    converter = Conv convert

instance Conv CompilationUnit Program where
    convert (CompilationUnit _ _ typeDecl) = OOProgram <$> mapConvert typeDecl
    
instance Conv TypeDecl Class where
    convert (ClassTypeDecl classDecl)  = convert classDecl
    convert (InterfaceTypeDecl _)      = fail "interface not supported"

instance Conv ClassDecl Class where
    convert (ClassDecl mdfs idt [] Nothing [] (ClassBody decls)) = Class <$> convert idt <*> mapConvert decls 
    convert ClassDecl {}                    = fail "this class not supported"
    convert EnumDecl {}                     = fail "Enum not supported"

instance Conv Decl ClassMember where
    convert (MemberDecl memberDecl) = convert memberDecl
    convert (InitDecl _ _)          = fail "Init not supported"

instance Conv MemberDecl ClassMember where
    convert memberDecl = case memberDecl of
         
        -- MethodDecl [Modifier] [TypeParam] (Maybe Type) Ident [FormalParam] [ExceptionType] (Maybe Exp) MethodBody
        MethodDecl mdfs [] mType ident formalParams [] Nothing body -> do
            rType <- case mType of 
                Nothing -> return Nothing
                Just t  -> Just <$> convert t
            fId'     <- convert ident
            fBody'   <- convert body
            fParams' <- mapConvert formalParams
            mdfs'    <- mapConvert mdfs
            return Method 
                { 
                    mReturnType = rType, mId = fId', mParams = fParams', 
                    mBody = fBody', mMdfs = mdfs'
                }

        MethodDecl {}           -> fail "This method is not supported"

        FieldDecl mdfs dt varDecls  -> Attribute <$> mapConvert mdfs <*> convert dt <*> mapConvert varDecls

        ConstructorDecl mdfs _ idt params [] (ConstructorBody _ body)      -> do
            mdfs'    <- mapConvert mdfs
            idt'     <- convert idt
            params'  <- mapConvert params
            body'    <- mapConvert body
            return $ Constructor mdfs' idt' params' (makeFragment body')
        
        ConstructorDecl {}  -> fail "This constructor not supported" 

        MemberClassDecl _       -> fail "memberclass not supported"  
        
        MemberInterfaceDecl _   -> fail "memberinterface not supported"

instance Conv MethodBody Fragment where
    -- TODO improve
    convert (MethodBody (Just (J.Block stmts)))    = x $ map (unCR . convert) stmts-- makeProgram <$> map (unCR . convert) stmts -- mapM convert stmts
     where
        x crs = if any isLeft crs
            then CR $ Left $ concatMap forceLeft (filter isLeft crs)
            else return (makeFragment $ map forceRight crs)
    convert (MethodBody Nothing)                   = return $ makeFragment []
    

instance Conv BlockStmt Statement where
    convert (BlockStmt stmt)             = convert stmt

    convert (LocalVars [] dt varDecls)  = case dt of
        RefType (J.ArrayType _) -> 
            case varDecls of 
                [v] -> ArrayDecls <$> convert dt <*> convert v
                _   -> fail "localvars with multiple array declarations not supported"
        _ -> VarDeclarations <$> convert dt <*> mapM convert varDecls

    convert (LocalVars mdfs _ _)         = fail "Local variables with modifiers not supported"

    convert (LocalClass classDecl)       = fail "Local class not supported"

instance Conv Stmt Statement where
    
    convert (IfThen exp stmt)              = If <$> convert exp <*> convert stmt
    convert (IfThenElse exp stmtT stmtF)   = IfElse <$> convert exp <*> convert stmtT <*> convert stmtF
    convert (J.While exp stmt)             = S.While <$> convert exp <*> convert stmt

    -- For ForInit [Expression] [Expression] Statement
    -- ->
    -- BasicFor (Maybe ForInit) (Maybe Exp) (Maybe [Exp]) Stmt
    convert (BasicFor forInit forCond forIt stmt) = For <$> 
        forInit' <*> forCond' <*> forIt' <*> convert stmt
        where
            forInit' = case forInit of
                Just init   -> convert init
                Nothing     -> pure $ ForInitExpr []
            forCond' = case forCond of
                Just exp   -> (:[]) <$> convert exp
                Nothing    -> pure []
            forIt' = case forIt of
                Just exps   -> mapM convert exps
                Nothing     -> pure []

    -- Expressions

    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    convert (ExpStmt (ArrayCreate dtype [Lit size] 0)) = ArrayDecls <$> convert dtype <*> pure (ArrDecl (Identifier "x"))
    convert (ExpStmt ArrayCreate {})        = fail "Array create not supported"

    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    convert (ExpStmt (ArrayCreateInit dtype int arrayInit)) 
        = ArrayDecls <$> convert dtype <*> pure (ArrInit (Identifier "x") $ LiteralExpr $ ArrayLit Seq.empty)

    convert (ExpStmt exp)                  = ExprStat <$> convert exp
    
    convert J.Empty                        = return S.Empty
    convert (StmtBlock (J.Block stmts))    = makeBlock <$> mapM convert stmts
    convert (J.Break Nothing)              = return S.Break 
    convert (J.Continue Nothing)           = return S.Continue
    convert (J.Continue _)                 = fail "Labeled continue not supported"
    convert (J.Break _)                    = fail "Labeled break not supported"

    convert (J.Return ex)                  = case ex of
        Nothing  -> S.Return <$> pure Nothing
        Just ex' -> S.Return . Just <$> convert ex'

    -- [Modifier] Type Ident Exp Stmt
    convert (EnhancedFor _ t idt ex stat)  = ForEach <$> convert t <*> convert idt 
                                                <*> convert ex <*> convert stat

    convert Assert {}                      = fail "Assert not supported"
    convert Switch {}                      = fail "Switch not supported"
    convert Do {}                          = fail "Do not supported"
    convert Synchronized {}                = fail "Synchronized not supported"
    convert Throw {}                       = fail "Throw not supported"
    convert Try { }                        = fail "Try not supported" 
    convert Labeled {}                     = fail "Labeled not supported"    


instance Conv Exp Expression where
    convert (Lit literal)                = LiteralExpr <$> convert literal

    convert (MethodInv methodInvocation) = convert methodInvocation
    
    convert (ExpName (Name [id1, id2]))  = Property <$> convert id1 <*> convert id2
    convert (ExpName name)               = IdExpr <$> convert name
    
    convert (PostIncrement exp)          = Postfixed Incr <$> convert exp
    convert (PostDecrement exp)          = Postfixed Decr <$> convert exp
    convert (PreIncrement exp)           = Prefixed Incr <$> convert exp
    convert (PreDecrement exp)           = Prefixed Decr <$> convert exp

    convert (PrePlus exp)                = Prefixed Plus <$> convert exp
    convert (PreMinus exp)               = Prefixed Minus <$> convert exp
    convert (PreNot exp)                 = Prefixed Not <$> convert exp
    convert (BinOp exp1 op exp2)         = Infixed <$> convert op <*> convert exp1 <*> convert exp2
    
    convert (J.Assign lhs assignOp exp)  = Assignment <$> convert assignOp <*> convert lhs <*> convert exp
    convert EHole                        = return $ HoleExpr 0

    convert (ArrayCreate dt [e] 0)       = NewArray <$> convert dt <*> convert e 
    convert ArrayCreate{}                = fail "Array creation not supported"

    convert (ArrayCreateInit dType int arrayInit)  = convert arrayInit
    
    convert (ArrayAccess (ArrayIndex (ExpName exp) [i])) = ArrayAcc <$> convert exp <*> convert i
    convert ArrayAccess {}               = fail $ "Array accessor not supported"
    
    convert Cond {}                      = fail $ "Conditional operator ? : not supported"
    convert Cast {}                      = fail $ "Casting not supported"

    convert Lambda {}                    = fail $ "Lambda not supported"
    convert MethodRef {}                 = fail $ "Method ref not supported"
    convert _                            = fail $ "Expression not supported"
    
    -- Expr below not supported
    --convert ClassLit (Maybe Type)
    --convert This
    --convert ThisClass Name
    --convert InstanceCreation [TypeArgument] ClassType [Argument] (Maybe ClassBody)
    --convert QualInstanceCreation Exp [TypeArgument] Ident [Argument] (Maybe ClassBody)
    --convert PreBitCompl Exp
    --convert FieldAccess
    --convert InstanceOf Exp RefType

instance Conv J.Literal S.Literal where
    convert (Int i)        = return $ IntLiteral (fromInteger i)
    convert (Boolean b)    = return $ BoolLiteral b
    convert (String s)     = return $ StringLiteral s
    convert J.Null         = return S.Null
    convert (Word _)       = fail "word not supported"
    convert (Float _)      = fail "float not supported" 
    convert (Double d)     = return $ DoubleLiteral d
    convert (Char _)       = fail "char not supported"

instance Conv Op InfixOp where
    convert Mult    = return Multiplication
    convert Div     = return Division
    convert Rem     = return Remainder
    convert Add     = return Addition
    convert Sub     = return Subtraction
    convert LShift  = fail "<< not supported"
    convert RShift  = fail ">> not supported"
    convert RRShift = fail ">>> not supported"
       
    convert LThan   = return Less
    convert GThan   = return Greater
    convert LThanE  = return LessOrEqual
    convert GThanE  = return GreaterOrEqual
    convert J.Equal = return S.Equal
    convert NotEq   = return NotEqual

    convert CAnd    = return AND
    convert COr     = return OR
    convert Xor     = fail "Xor not supported"
    convert And     = fail "& not supported"
    convert Or      = fail "| not supported"

instance Conv J.AssignOp S.AssignOp where
    convert EqualA     = return S.Assign 
    convert MultA      = return AssignMul
    convert DivA       = return AssignDiv
    convert RemA       = return AssignRem
    convert AddA       = return AssignAdd
    convert SubA       = return AssignSub
              
    convert LShiftA    = fail "<<= not supported"
    convert RShiftA    = fail ">>= not supported"
    convert RRShiftA   = fail ">>>= not supported"
    convert AndA       = fail "&= not supported"
    convert XorA       = fail "^= not supported"
    convert OrA        = fail "|= not supported"

instance Conv Name Identifier where
    -- A name, i.e. a period-separated list of identifiers.
    convert (Name ids) = return $ Identifier $ intercalate "." (map getS ids)
        where
            getS (Ident s) = s

instance Conv MethodInvocation Expression where

    -- | Invoking a specific named method.
    convert (MethodCall name args) = Call <$> convert name <*> mapM convert args
    convert _                       = fail "Method call not supported"

    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    -- | PrimaryMethodCall Exp [RefType] Ident [Argument]
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    -- | SuperMethodCall [RefType] Ident [Argument]
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    -- | ClassMethodCall Name [RefType] Ident [Argument]
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    -- | TypeMethodCall  Name [RefType] Ident [Argument]

instance Conv Lhs Expression where
    convert (NameLhs name) = IdExpr <$> convert name -- ^ Assign to a variable
    convert (ArrayLhs (ArrayIndex (ExpName name) [exp])) = ArrayAcc <$> convert name <*> convert exp 
    convert (ArrayLhs _)   = fail "ArrayLhs not supported"  
    convert _              = fail "Lhs not supported"
    -- | FieldLhs FieldAccess  -- ^ Assign through a field access
    

instance Conv Type DataType where
    convert (PrimType primType)    = convert primType
    convert (RefType refType)      = convert refType

instance Conv PrimType DataType where
    convert t = case t of 
        BooleanT -> return BoolType
        IntT     -> return IntType
        CharT    -> fail "char not supported"
        FloatT   -> fail "float not supported"
        DoubleT  -> return DoubleType
        _        -> fail "Unsupported primitive type"
        {-ByteT  
        ShortT
        LongT-}

instance Conv RefType DataType where
    convert (J.ArrayType tp) = S.ArrayType <$> convert tp
    convert (ClassRefType (ClassType [(Ident "String", [])])) = return StringType 
    convert (ClassRefType (ClassType [(Ident s, [])])) = fail $ "Unsupported class " ++ show s
    convert (ClassRefType _) = fail $ "Unsupported type"

instance Conv J.ForInit S.ForInit where
    convert (ForLocalVars [] tp varDecls)  = ForInitDecls <$> convert tp <*> mapM convert varDecls
    convert (ForLocalVars _ _ _)           = fail "Modifiers in for init unsupported"
    convert (ForInitExps exps)             = ForInitExpr <$> mapM convert exps

    -- |   FHole               LocationID

instance Conv VarDecl ArrayDecl where
    convert (VarDecl varDeclId (Just varInit)) = ArrInit <$> (getIdt <$> convert varDeclId) <*> convert varInit
    convert (VarDecl varDeclId Nothing)        = ArrDecl . getIdt <$> convert varDeclId 

instance Conv VarDecl Expression where
    convert (VarDecl varDeclId (Just varInit)) = Assignment S.Assign <$> convert varDeclId <*> convert varInit
    convert (VarDecl varDeclId Nothing)        = convert varDeclId

instance Conv VarDeclId Expression where
    -- | The name of a variable in a declaration, which may be an array, 'int[] a' syntax
    convert (VarId ident)                = IdExpr <$> convert ident
    -- used for 'int a[]' syntax
    convert (VarDeclArray (VarId ident)) = IdExpr <$> convert ident
    convert (VarDeclArray _)             = fail "Array syntax not supported"
    --  
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.

instance Conv VarInit Expression where
-- | Explicit initializer for a variable declaration.
    convert (InitExp (ArrayCreate dt [Lit (Int size)] 0))  = do
        dt' <- convert dt
        return . LiteralExpr . ArrayLit $ initArray (fromInteger size) dt'
    convert (InitExp exp)           = convert exp
    convert (InitArray arrayInit)   = convert arrayInit
    
instance Conv Ident Identifier where
    convert (Ident str) = return $ Identifier str

instance Conv ArrayInit Expression where
    convert (ArrayInit [])       = return . LiteralExpr . ArrayLit $ Seq.fromList []
    convert (ArrayInit varInits) = case head varInits of
        InitExp (Lit _) -> LiteralExpr . ArrayLit . Seq.fromList . map (\(LiteralExpr l) -> l) <$> mapConvert varInits
        _               -> fail "Array init not supported"


instance Conv FormalParam Param where
    convert (FormalParam [] dType False (VarId ident)) = Param <$> convert dType <*> convert ident
    convert _ = fail "Param not supported"

instance Conv J.Modifier S.Modifier where
    convert mdf = case mdf of
        J.Public  -> return S.Public
        J.Private -> return S.Private
        J.Static  -> return S.Static
        J.Final   -> return S.Final
        _ -> fail ("Modifier not supported")