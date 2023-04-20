------------------------------------------------------------------
-- Syntax
--
-- Created: 9-9-13, modified x-x-x 
--
------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, StandaloneDeriving, UndecidableInstances, LambdaCase #-}

module Domain.Syntax 
where

import Data.Data hiding (DataType)
import Data.Maybe
import Data.Semigroup
import Data.Generics.Uniplate.DataOnly (universeBi)
import Data.Generics.Uniplate.Direct hiding (universeBi)
import qualified Data.Sequence as S
import Ideas.Common.Library (Difficulty(..), IsTerm, toTerm, fromTerm) --, , fromTermG, toTermG)
import Control.Monad

{-!

deriving instance UniplateDirect UnaryOp
deriving instance UniplateDirect Literal

!-}

--deriving instance UniplateDirect Program
--deriving instance UniplateDirect Statement
--deriving instance UniplateDirect Expression
--deriving instance UniplateDirect InfixOp
--deriving instance UniplateDirect DataType
--deriving instance UniplateDirect AssignOp
--deriving instance UniplateDirect UnaryOp
--deriving instance UniplateDirect Identifier
--deriving instance UniplateDirect S.Seq Literal
--deriving instance UniplateDirect Literal
--deriving instance UniplateDirect Statement Identifier

--------------------------------------------------------------------------------
-- Data types

type LocationID = Int
type Array = S.Seq Literal

data DataType =     
        BoolType 
    |   IntType 
    |   StringType 
    |   ArrayType DataType 
    |   DoubleType
    deriving (Data, Typeable, Eq, Show, Ord, Read)

data Literal = 
        Null
    |   IntLiteral      Int
    |   BoolLiteral     Bool
    |   StringLiteral   String 
    |   ArrayLit        Array
    |   DoubleLiteral   Double
    deriving (Data, Typeable, Eq, Ord, Show)

data InfixOp = 
        Addition | Subtraction | Multiplication | Division | Remainder 
    |   Equal | NotEqual | Greater | GreaterOrEqual | Less | LessOrEqual 
    |   AND | OR | BAnd
    deriving (Data, Typeable, Eq, Show, Ord)

data AssignOp = 
        Assign 
    |   AssignMul | AssignAdd | AssignSub | AssignDiv | AssignRem
    deriving (Data, Typeable,  Eq, Show, Ord)
    
data UnaryOp = 
        Minus | Plus | Not | Incr | Decr 
    deriving (Data, Typeable, Eq, Show, Ord)

data Identifier = Identifier { name :: String } 
    deriving (Data, Typeable, Eq, Show, Ord, Read)

data Program = Program 
    {
        body        :: Statement, 
        desc        :: String, 
        difficulty  :: Difficulty,
        preference  :: Int
    } 
    | OOProgram [Class]
    deriving (Data, Typeable,  Eq, Show, Ord)

deriving instance Data Difficulty

data Statement = 
        Block               LocationID [Statement]
    |   If                  Expression Statement
    |   IfElse              Expression Statement Statement
    |   While               Expression Statement
    |   For                 ForInit [Expression] [Expression] Statement
    |   ForEach             DataType Identifier Expression Statement         
    |   Print               Expression
    |   VarDeclarations     DataType [Expression]
    |   ArrayDecls          DataType ArrayDecl
    |   Empty  
    |   ExprStat            Expression
    |   Break
    |   Continue
    |   Function            { returnType :: Maybe DataType, fId :: Identifier, fParams :: [Param], fBody :: Fragment }
    |   Return              (Maybe Expression) 
    -- Annotations
    |   Feedback            String Statement
    |   MustUse             Statement
    |   Alt                 [Statement]
    deriving (Data, Typeable, Eq, Show, Ord)

data Fragment = Fragment 
    { 
        fLocId :: LocationID, 
        stats :: [Statement] 
    } deriving (Data, Typeable, Eq, Show, Ord)

data ForInit = 
        ForInitExpr         [Expression] 
    |   ForInitDecls        DataType [Expression]
    |   FHole               LocationID
    deriving (Data, Typeable, Eq, Show, Ord)

data Expression =   
        Infixed     InfixOp Expression Expression
    |   Assignment  AssignOp Expression Expression -- eigenlijk Id=expr
    |   Prefixed    UnaryOp Expression
    |   Postfixed   UnaryOp Expression
    |   LiteralExpr Literal
    |   IdExpr      Identifier
    |   ArrayAcc    Identifier Expression 
    -- to do array init
    |   HoleExpr    LocationID
    |   Call        Identifier [Expression] 
    |   Property    Identifier Identifier -- TODO Expression Identifier
    |   NewArray    DataType Expression
    |   Ternary     Expression Expression Expression
    deriving (Data, Typeable, Eq, Ord, Show)

data Param = 
        Param DataType Identifier 
    |   PHole LocationID
    deriving (Data, Typeable, Eq, Ord, Show)

data ArrayDecl = 
        ArrDecl         Identifier
    |   ArrInit         Identifier Expression
    deriving (Data, Typeable, Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Terms

{-instance IsTerm Program where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm Statement where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm Fragment where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm Expression where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm ForInit where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm Identifier where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm DataType where
    toTerm   = toTermG
    fromTerm = fromTermG

instance IsTerm ClassMember where
    toTerm   = toTermG
    fromTerm = fromTermG-}

--------------------------------------------------------------------------------
-- OO programs

data Class = Class Identifier [ClassMember] deriving (Data, Typeable, Eq, Show, Ord)

data ClassMember = 
        Attribute [Modifier] DataType [Expression] 
    |   Method 
        { 
            mReturnType :: Maybe DataType, 
            mId :: Identifier, 
            mParams :: [Param], 
            mBody :: Fragment,
            mMdfs :: [Modifier]
        }
    |   Constructor [Modifier] Identifier [Param] Fragment
    deriving (Data, Typeable,  Eq, Show, Ord)

data Modifier = 
        Public
    |   Private
    |   Static
    |   Final
    deriving (Data, Typeable,  Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Utils

-- Programs --------------------------------------------------------------------

-- Used in program transformations
-- check
class (Eq a, Data a, Show a) => IsProgram a where
    toP  :: a -> [Statement] 
    mapP :: ([Statement] -> [Statement]) -> a -> a

    statAt :: Int -> a -> Maybe Statement
    statAt i p 
        | i < length stts = Just (stts !! i)
        | otherwise        = Nothing
        where
            stts = toP p 

    params :: a -> [Param]
    params _ = []
    
instance IsProgram Program where
    toP Program { body = b } = getBlockStats b
    toP OOProgram {}         = error "no toP for OOProgram"

    mapP f p = p { body = mapP f (body p) }

instance IsProgram Statement where
    toP (Block _ stats) = stats
    toP s = [s]
    mapP f (Block nr stts) = Block nr (f stts)
    mapP f s = Block 0 $ f [s] 
 
instance IsProgram Fragment where
    toP = stats
    mapP f frag = Fragment (fLocId frag) . f . toP $ frag

instance IsProgram ClassMember where
    toP Method { mBody = b } = toP b 
    toP _                    = error "no toP for ClassMember"
    params p = case p of
        Method { }           -> mParams p
        Constructor _ _ ps _ -> ps
        _                    -> []
    mapP                     = error "no mapP for ClassMember"

-- | Find the main method and extract its code. 
-- Program should have one class with only a main-method
stripMainCode :: Program -> Maybe Program
stripMainCode program = case program of
    OOProgram [Class _ [
        Method 
        {
            mReturnType = Nothing,
            mId = Identifier "main",
            mParams = [Param (ArrayType StringType) (Identifier _)],
            mBody = body,
            mMdfs = [Public, Static]
        } ]
        ] -> Just . makeProgram $ stats body
    OOProgram _ -> Nothing
    Program {} -> Just program             

-- | Find the method with name and extract its code. 
-- Program should have one class
stripMethodByName :: String -> Program -> Maybe ClassMember
stripMethodByName mName = mfilter (\m -> name (mId m) == mName) . stripMethod

-- | Find the single method and extract its code. 
-- Program should have one class
stripMethod :: Program -> Maybe ClassMember
stripMethod program = case program of
    OOProgram [Class _ [m@Method {}]] -> Just m
    _ -> Nothing

isMethod :: ClassMember -> Bool
isMethod Method {} = True
isMethod _         = False

methodName :: ClassMember -> Maybe String
methodName m = case m of
    Method {} -> Just (name $ mId m)
    _         -> Nothing

makeEmptyProgram :: Program
makeEmptyProgram = Program 
    {
        body = Block 0 [], 
        desc = "", 
        preference = 0,
        difficulty = Medium
    }

makeEmptyOOProgram :: Program
makeEmptyOOProgram = OOProgram []

makeOOProgram :: [Class] -> Program
makeOOProgram = OOProgram

makeClass :: Identifier -> [ClassMember] -> Class
makeClass = Class

makeProgram :: [Statement] -> Program
makeProgram stts = makeEmptyProgram { body = makeBlock stts }

getBlockStats :: Statement -> [Statement]
getBlockStats (Block _ stts) = stts  
getBlockStats s               = [s]

makeBlockWithId :: Int -> Statement
makeBlockWithId = flip Block []

makeBlock :: [Statement] -> Statement
makeBlock = Block 0

makeFragmentWithId :: Int -> Fragment
makeFragmentWithId = flip Fragment []

makeFragment :: [Statement] -> Fragment
makeFragment = Fragment 0

emptyFragment :: Fragment
emptyFragment = makeFragment []

programToFragment :: Program -> Fragment
programToFragment = makeFragment . toP

changeFragment :: Fragment -> [Statement] -> Fragment
changeFragment f newS = f { stats = newS }

-- | Counts the number of blocks & fragments with a certain id
nrOfBlocksById :: LocationID -> Program -> Int
nrOfBlocksById nr p = length [i | Block i _ <- universeBi p, i == nr]
    + length [i | Fragment i _ <- universeBi p, i == nr]

-- | Returns a block with a given locationid, if there is only one occurence
findBlock :: LocationID -> Program -> Maybe Statement
findBlock nr p  
    | length blocksWithNr == 1  = Just $ head blocksWithNr 
    | otherwise                 = Nothing
    where
        blocksWithNr = [ b | b@(Block i _) <- universeBi p, i == nr]
     
subProgram :: Int -> Program -> Program
subProgram loc p = p { body = newBody (body p) }
    where 
        newBody (Block a b) = Block a $ take loc b
        x = mapProgram 

nrOfSubs :: Program -> Int
nrOfSubs = mapProgram length
 
mapProgram :: ([Statement] -> a) -> Program ->  a
mapProgram f = f . getStts . body 
    where
        getStts (Block _ xs) = xs
        getStts x = [x]

-- Statements ------------------------------------------------------------------

instance Monoid Statement where
    mempty                              = Empty
    mappend Empty s                     = s
    mappend s Empty                     = s
    mappend (Block nr xs) (Block _ ys)  = Block nr $ xs ++ ys  
    mappend (Block nr xs) s             = Block nr $ xs ++ [s]
    mappend s (Block nr xs)             = Block nr $ s:xs
    mappend s1 s2                       = Block 0 $ s1:[s2]

instance Semigroup Statement where
   (<>) = mappend

forInitToStat :: ForInit -> [Statement]
forInitToStat (ForInitExpr exprs)       = map ExprStat exprs
forInitToStat (ForInitDecls dt exprs)   = [VarDeclarations dt exprs]
forInitToStat (FHole nr)                = [ExprStat (HoleExpr nr)]

statToForInit :: Statement -> Maybe ForInit
statToForInit (ExprStat (HoleExpr nr))   = Just (FHole nr)
statToForInit (ExprStat e)               = Just (ForInitExpr [e])
statToForInit (VarDeclarations dt exprs) = Just (ForInitDecls dt exprs)
statToForInit _                          = Nothing

isAnnotation :: Statement -> Bool
isAnnotation Feedback {}    = True
isAnnotation MustUse {}     = True
isAnnotation Alt {}         = True
isAnnotation _              = False

isBlock :: Int -> Program -> Bool
isBlock nr p = nr `elem` [i | Block i _ <- universeBi p]

-- | Returns all stats
getStats :: Program -> [Statement]
getStats = universeBi

-- | returns the identifiers that are declared but not initialised
getDeclNoInit :: Statement -> [Identifier]
getDeclNoInit (VarDeclarations _ e) = mapMaybe declVar e
    where      
        declVar (IdExpr i)  = Just i
        declVar _           = Nothing
getDeclNoInit _ = []

isIfElse :: Statement -> Bool
isIfElse IfElse {} = True 
isIfElse _         = False

isIf :: Statement -> Bool
isIf If {} = True 
isIf _     = False

isIforIfElse :: Statement -> Bool
isIforIfElse If {}     = True 
isIforIfElse IfElse {} = True 
isIforIfElse _         = False

isWhile :: Statement -> Bool
isWhile = \case
    While {} -> True 
    _        -> False

isFor :: Statement -> Bool
isFor For {} = True 
isFor _      = False

emptyForInit :: ForInit
emptyForInit = ForInitExpr []

-- Data types ------------------------------------------------------------------


defaultValue :: DataType -> Literal
defaultValue BoolType       = fLit
defaultValue IntType        = iLit0
defaultValue StringType     = emptySLit
defaultValue (ArrayType _)  = Null
defaultValue DoubleType     = dLit0
defaultValue _              = error "no default for type" 

litType :: Literal -> Maybe DataType
litType = \case
    Null             -> Nothing 
    IntLiteral {}    -> Just IntType
    BoolLiteral {}   -> Just BoolType
    StringLiteral {} -> Just StringType
    DoubleLiteral {} -> Just DoubleType
    ArrayLit a       -> if S.null a then Nothing else Nothing -- TODO

mayBeNull :: DataType -> Bool
mayBeNull StringType     = True
mayBeNull (ArrayType _)  = True
mayBeNull _              = False

isArrType :: DataType -> Bool
isArrType (ArrayType _) = True
isArrType _ = False

getArrType :: DataType -> Maybe DataType
getArrType (ArrayType dt) = Just dt
getArrType _ = Nothing

getArrBaseType :: DataType -> Maybe DataType
getArrBaseType t = case t of
    ArrayType dt -> Just (getArrBaseType' dt)
    _            -> Nothing
    where
        getArrBaseType' (ArrayType dt) = getArrBaseType' dt
        getArrBaseType' dt             = dt

-- Does not take scope into account
findDataType :: IsProgram p => Identifier -> p -> Maybe DataType
findDataType idt = lookup idt . findDataTypes

-- | Returns a list of all declared variables and their type
findDataTypes :: IsProgram p => p -> [(Identifier, DataType)]
findDataTypes p = 
       findArrDataTypes p
    ++ unfold [ (mapMaybe declaredIdt decls, dt) | VarDeclarations dt decls <- universeBi (toP p)]
    ++ [ (idt, dt) | Param dt idt <- params p, not (isArrType dt) ]
    where
        unfold = concatMap (\(a, b) -> zip a (repeat b))

-- | Returns a list of all declared array variables and their type (ArrayType X)
findArrDataTypes :: IsProgram p => p -> [(Identifier, DataType)]
findArrDataTypes p = 
       [ (declaredArrIdt decls, dt) | ArrayDecls dt decls <- universeBi (toP p)]
    ++ [ (idt, dt) | Param dt idt <- params p, isArrType dt]

-- | Returns declared variables (not arrays), e.g. int x = ..; and int x;
declaredIdt :: Expression -> Maybe Identifier
declaredIdt (Assignment Assign (IdExpr idt) _) = Just idt
declaredIdt (IdExpr idt)                       = Just idt
declaredIdt _                                  = Nothing

-- | Returns declared array variables, e.g. int [] x = ..; and int [] x;
declaredArrIdt :: ArrayDecl -> Identifier
declaredArrIdt (ArrDecl idt)   = idt
declaredArrIdt (ArrInit idt _) = idt


-- Identifiers ------------------------------------------------------------------

instance Monoid Identifier where
    mempty       = makeIdentifier ""
    mappend a b  = makeIdentifier (name a ++ name b)

instance Semigroup Identifier where
    (<>) = mappend

makeIdentifier :: String -> Identifier
makeIdentifier n = Identifier { name = n }

prependIdt :: String -> Identifier -> Identifier
prependIdt s idt = makeIdentifier s <> idt

appendIdt :: String -> Identifier -> Identifier
appendIdt s idt = idt <> makeIdentifier s

getIdt :: Expression -> Identifier
getIdt (IdExpr i) = i
getIdt _          = error "Not an Identifier"

isIdt :: Expression -> Bool
isIdt (IdExpr i) = True
isIdt _          = False

-- Literals --------------------------------------------------------------------

isInt, isString, isBool, isArray, isDouble :: Literal -> Bool
isInt (IntLiteral _)    = True
isInt _                 = False

isString (StringLiteral _)  = True
isString _                  = False

isBool (BoolLiteral _)  = True
isBool _                = False

isArray (ArrayLit _)    = True
isArray _               = False

isDouble (DoubleLiteral _) = True
isDouble _                 = False

getArray :: Literal -> Maybe Array
getArray (ArrayLit a) = Just a
getArray _ = Nothing

initArray :: Int -> DataType -> Array
initArray size = S.replicate size . defaultValue

makeIntArray :: [Int] -> Literal
makeIntArray = ArrayLit . S.fromList . map iLit

class Inc a where
    incExpr :: Int -> a -> a

instance Inc Expression where
    incExpr i (LiteralExpr (IntLiteral x)) = LiteralExpr $ IntLiteral $ x + i
    incExpr i x =  x .+. (LiteralExpr . IntLiteral $ i )



--------------------------------------------------------------------------------
-- Operators

isComparisonOp, isArithmeticOp, isBoolOp, isBitwiseOp :: InfixOp -> Bool
isComparisonOp = flip elem [ Equal, NotEqual, Greater, GreaterOrEqual, Less, LessOrEqual]
isArithmeticOp = flip elem [Addition, Multiplication, Division, Subtraction, Remainder]
isBoolOp       = flip elem [AND, OR]
isBitwiseOp    = flip elem [BAnd]

class HasPrecedence a where
    precedence  :: a -> Int

instance HasPrecedence InfixOp where
    precedence op = fromMaybe 99 $ lookup op precedenceMap

instance HasPrecedence UnaryOp where
    precedence op = fromMaybe 99 $ lookup op unPrecedenceMap

getOpPrec :: Expression -> Maybe Int
getOpPrec (Infixed op _ _ ) = Just (precedence op)
getOpPrec (Prefixed op _ )  = Just (precedence op)
getOpPrec (Postfixed op _ ) = Just (precedence op)
getOpPrec _                 = Nothing

getCompl :: InfixOp -> Maybe InfixOp
getCompl Addition       = Just Subtraction
getCompl Subtraction    = Just Addition
getCompl Division       = Just Multiplication
getCompl Multiplication = Just Division
getCompl Equal          = Just NotEqual
getCompl NotEqual       = Just Equal
getCompl Less           = Just GreaterOrEqual
getCompl LessOrEqual    = Just Greater
getCompl GreaterOrEqual = Just Less
getCompl Greater        = Just LessOrEqual
getCompl AND            = Nothing
getCompl OR             = Nothing
getCompl Remainder      = Nothing
getCompl BAnd           = Nothing

precedenceMap :: [(InfixOp, Int)]
precedenceMap = 
    [ (Division, 6), (Multiplication, 6), (Remainder, 6)
    , (Addition, 7), (Subtraction, 7), (BAnd, 7)
    , (Less, 9), (LessOrEqual, 9), (Greater, 9), (GreaterOrEqual, 9) 
    , (Equal, 10), (NotEqual, 10)
    , (AND, 14), (OR, 15)
    ]

unPrecedenceMap :: [(UnaryOp, Int)]
unPrecedenceMap = 
    [ (Minus, 5), (Plus, 5), (Not, 5)
    , (Incr, 4), (Decr, 4)
    ]

-- Boolean

tLit, fLit :: Literal
tLit = BoolLiteral True
fLit = BoolLiteral False

(.||.), (.&&.), (.+.), (.-.), (./.), (.%.), (.==.), (.!=.), (.=.), (.+=.),
    (.-=.), (.*=.), (./=.), (.%=.) :: Expression -> Expression -> Expression

(.||.)  = Infixed OR
(.&&.)  = Infixed AND
(.+.)   = Infixed Addition
-- (.*.)   = Infixed Multiplication
(.-.)   = Infixed Subtraction
(./.)   = Infixed Division
(.%.)   = Infixed Remainder
(.==.)  = Infixed Equal
(.!=.)  = Infixed NotEqual
(.=.)   = Assignment Assign
(.+=.)  = Assignment AssignAdd
(.-=.)  = Assignment AssignSub
(.*=.)  = Assignment AssignMul
(./=.)  = Assignment AssignDiv
(.%=.)  = Assignment AssignRem

nt :: Expression -> Expression
nt      = Prefixed Not

trueLt, falseLt :: Expression
trueLt  = LiteralExpr tLit
falseLt = LiteralExpr fLit

-- Arithmetic

iLit :: Int -> Literal
iLit  = IntLiteral

iLit0, iLit1, iLit2 :: Literal
iLit0 = iLit 0
iLit1 = iLit 1
iLit2 = iLit 2

i0Lt, i1Lt, i2Lt :: Expression
i0Lt  = LiteralExpr iLit0
i1Lt  = LiteralExpr iLit1
i2Lt  = LiteralExpr iLit2

dLit :: Double -> Literal
dLit = DoubleLiteral
dLit0, dLit1 :: Literal
dLit0 = dLit 0.0
dLit1 = dLit 1.0

(.<.), (.<=.), (.>=.), (.>.) :: Expression -> Expression -> Expression
(.<.)   = Infixed Less
(.<=.)  = Infixed LessOrEqual
(.>.)   = Infixed Greater
(.>=.)  = Infixed GreaterOrEqual
(.++.), (.--.), (-.) :: Expression -> Expression
(.++.)  = Postfixed Incr
(.--.)  = Postfixed Decr
(-.)   = Prefixed Minus

emptySLit :: Literal
emptySLit = StringLiteral ""

-- Expressions -----------------------------------------------------------------

instance Num Expression where
    fromInteger x   = makeInt $ fromInteger x
    x + y           = x .+. y
    x * y           = Infixed Multiplication x y
    x - y           = Infixed Subtraction x y
    negate          = Prefixed Minus
    abs             = undefined
    signum          = undefined

class PExpr a where
    lit0, lit1, trueLit, falseLit, emptyString :: a
    isLit :: a -> Bool
    makeInt :: Int -> a
    makeIdt :: String -> a
    isExprHole :: a -> Bool
    
instance PExpr Expression where
    lit0        = makeInt 0
    lit1        = makeInt 1
    trueLit     = trueLt  
    falseLit    = falseLt
    emptyString = LiteralExpr emptySLit

    isLit (LiteralExpr _) = True
    isLit _ = False

    makeInt = LiteralExpr . iLit 
    makeIdt = IdExpr . Identifier 
    
    isExprHole (HoleExpr _) = True
    isExprHole _ = False  

isVarAssign :: Statement -> Bool
-- isVarAssign (VarDeclarations _ _) = True
isVarAssign (ExprStat Assignment {} ) = True
isVarAssign _ = False

getName :: Param -> String
getName (Param _ pId) = name pId
getName PHole {}     = "?"

paramType :: Param -> DataType
paramType (Param dt _) = dt
paramType PHole {}     = error "phole"


-- Literals --------------------------------------------------------------------

-- Used in evaluator
class ToOutput a where
    toOutput :: a -> String
    
instance ToOutput Literal where
    toOutput (BoolLiteral b)    = show b
    toOutput (IntLiteral i)     = show i
    toOutput (DoubleLiteral d)  = show d
    toOutput (StringLiteral s)  = s
    toOutput Null               = "null"
    toOutput (ArrayLit _)       = "@array@"

--------------------------------------------------------------------------------
-- Holes

class Holed a where
    containsHole :: a -> Bool
 
instance Holed Program where
    containsHole = not . null . allHoles 

class CanBeHole a where
    isHole :: a -> Bool
    getLocationId :: a -> Maybe LocationID 

    isHole = isJust . getLocationId

instance CanBeHole Param where
    getLocationId (PHole loc)   = Just loc
    getLocationId _     = Nothing
 
instance CanBeHole Expression where
    getLocationId (HoleExpr loc) = Just loc
    getLocationId _     = Nothing

instance CanBeHole ForInit where
    getLocationId (FHole loc)   = Just loc
    getLocationId _     = Nothing

-- | Counts the number of holes with a certain id
nrOfHolesById :: LocationID -> Program -> Int
nrOfHolesById nr = length . filter (==nr) . allHoles

-- | Total number of holes
nrOfHoles :: (Data a) => a -> Int
nrOfHoles = length . allHoles

-- | Get the id's of all holes
allHoles :: (Data a) => a -> [LocationID]
allHoles s = [i | HoleExpr i <- universeBi s] ++ [i | FHole i <- universeBi s] ++ [i | PHole i <- universeBi s] 

------------------------------------------------------------------------------


-- GENERATED START

 
instance Uniplate UnaryOp where
         
        {-# INLINE uniplate #-}
        uniplate x = plate x
-- GENERATED STOP
