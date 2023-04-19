{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FunctionalDependencies, FlexibleContexts #-}
------------------------------------------------------------------
-- Evaluator
--
-- Created: 9-10-13, modified x-x-x 
--
------------------------------------------------------------------

-- TODO eval scope?
-- TODO Int --> Integer?

module Domain.Evaluator 
(
    evalProgram, EvalResult(..), calc-- evalCode
    , testMethodWith, testMethodWith1, testMethodWithSucceeds, evalMethod
    , TestCase(..), showTC, makeTC
    , eval
)
where

import Domain.Syntax as P hiding (Statement(..), Expression(..), ForInit(..), ClassMember(..), AssignOp(..), params) 
import Domain.Base.AST as B
import Domain.Base.Conversion
import Domain.Printer
import Data.Maybe
import Data.Either
import Control.Monad.State hiding (state)
import qualified Data.Map as Map
import Control.Monad.Except
import qualified Data.Sequence as S
import Text.PrettyPrint.Leijen hiding ((<$>))
import Domain.Parsers.JavaParser
import Data.Bits ((.&.), Bits)

evalProgram :: ToBase a => a -> Either EvalResult String
evalProgram p = case result of
    Left x   -> Left x
    Right () -> Right $ getOutput finalState
    where 
        (result, finalState) = runState (state p) emptyEvalStat
        getOutput = concat . output
        state = runExceptT . unEval . eval . toB

-- simple input/output testing
type TestInput = [Literal]
data TestCase = TestCase { testIn :: TestInput, testOut :: Literal, testMsg :: Maybe String } deriving Show

makeTC :: (TestInput, Literal) -> TestCase
makeTC (i,o) = TestCase i o Nothing

showTC :: TestCase -> Doc
showTC tc = brackets (prettyCommaList (map pretty (testIn tc))) <+> text "should return" <+> pretty (testOut tc)

-- | Do all test cases succeed?
testMethodWithSucceeds :: BClassMember -> [TestCase] -> Bool
testMethodWithSucceeds m = (\res -> null (lefts res) && and (rights res)) . testMethodWith m

testMethodWith :: BClassMember -> [TestCase] -> [Either EvalResult Bool]
testMethodWith method = map (testMethodWith1 method)

testMethodWith1 :: BClassMember -> TestCase -> Either EvalResult Bool
testMethodWith1 method tc = do
    -- pre check method return type + expected output type
    case method of
        BMethod {} -> 
            when (not . typeMatch $ mReturnType method) $ Left (EvalError "Test cases do not match the return type of the method")
        _ -> return ()
    -- run test
    maybeLit <- evalMethod method tInput
    -- Check actual return type
    when (not . typeMatch $ (maybeLit >>= litType)) $ Left (EvalError "Test cases do not match the returned type")
    return $ maybeLit == Just tOutput
    where
        tInput  = testIn tc
        tOutput = testOut tc
        tOutputType = litType tOutput
        typeMatch :: Maybe DataType -> Bool
        typeMatch t = t == tOutputType || (isNothing tOutputType && maybe False mayBeNull t)


evalMethod :: BClassMember -> TestInput -> Either EvalResult (Maybe Literal)
evalMethod m inputVals = fst $ runState (state m) (emptyEvalStat { input = inputVals})
    where 
        state = runExceptT . unEval . eval

---------------------------------------------------------------
-- Types

type VarName = String
type VarList = Map.Map VarName Literal

data EvalResult = 
        BreakEncountered 
    |   ContinueEncountered
    |   NotSupported String
    |   EvalError String
    |   ReturnEncountered (Maybe Literal)
    deriving Eq

instance Show EvalResult where
    show (EvalError s)    = s
    show (NotSupported s) = "Unable to test, unsupported " ++ s
    show s                = "unknown EvalResult"

newtype Eval a = Eval { unEval :: ExceptT EvalResult (State EvalState) a }
    deriving (Monad, MonadState EvalState, MonadError EvalResult, Applicative, Functor) -- TODO app/func??

data EvalState = EvalState 
    {   environment :: VarList
      , output      :: [String]
      , input       :: [Literal]      -- for testing functions
      , program     :: Maybe BProgram
    } deriving Show

emptyEvalStat :: EvalState
emptyEvalStat = EvalState 
    {   environment = Map.empty
      , output      = []
      , input       = []
      , program     = Nothing 
    }

class Evaluation a b | a -> b where
    eval :: a -> Eval b
   
--------------------------------------------------------------------------------
-- Statement evaluators

instance Evaluation BProgram () where
    eval p = case p of
        B.Program {}   -> do
            modify (\s -> s { program = Just p }) -- store the program in the state
            (eval . bbody) p

        B.OOProgram {} -> return () -- No evaluation for OO programs

instance Evaluation [BStat] () where
    eval = mapM_ eval 
       
instance Evaluation BStat () where
    eval stat = case stat of
        Print e -> 
            do
                outputLit <- eval e
                addToOutput outputLit
        
        ExprStat e -> void $ eval e
                
        Block xs -> mapM_ eval xs
        
        Assignment (IdExpr idf) e2 -> 
            do
                e2' <- eval e2
                addToMap(name idf) Nothing e2'
        
        Assignment (ArrayAcc idf idx) e2 -> 
            do
                e2' <- eval e2
                index <- eval idx >>= toInt
                addToMap (name idf) (Just index) e2'

        IfElse e s1 s2  -> 
            do
                doIf <- checkBoolExpr [e] "No boolean condition in if"
                eval $ if doIf then s1 else s2
                    
        While condition wBody ->  loop [condition] (eval wBody)
    
        Break -> throwError BreakEncountered -- caught in loop
        
        Continue -> throwError ContinueEncountered -- caught in loop
        
        Return re -> case re of 
            Nothing -> throwError (ReturnEncountered Nothing) -- void
            Just e  -> do
                retVal <- eval e
                throwError $ ReturnEncountered (Just retVal) -- caught by Method

        unknown -> throwError $ NotSupported (show unknown)
        
        -- TODO remove?
        `catchError` (\err ->
            case err of 
                _ -> throwError err
        )
        
        where
            -- error if no boolean expression found    
            checkBoolExpr tests msg = 
                do
                    evals <- mapM eval tests >>= mapM toBool            
                    return $ and evals
                    `catchError`  const (throwEvalError msg)
                               --  (\err -> throwError err)
            
            -- returns True if no boolean expression found       
            checkBoolExpr' tests = 
                do
                    evals <- mapM eval tests >>= mapM toBool            
                    return $ and evals
                    `catchError` const (return True)
                                
            loop tests lBody = loop' (1 :: Integer)
                where 
                    loop' counter = 
                        do
                            when (counter > 99999) $ throwEvalError "Your loop seems to be infinite, check the stop condition!"
                            doLoop <- checkBoolExpr' tests -- "No boolean condition in loop"
                            when doLoop $ lBody >> loop' (counter + 1)
                            `catchError` (\err -> 
                                case err of 
                                    BreakEncountered    -> return ()
                                    ContinueEncountered -> loop' (counter + 1)
                                    _                   -> throwError err
                            )
                            

--------------------------------------------------------------------------------
-- Expression evaluator

instance Evaluation BExpr Literal where
    eval expr = case expr of
                  
        Infixed op e1 e2 ->
            do  
                e1' <- eval e1
                case op of 
                    -- AND + OR are short-circuit operators in Java
                    AND | e1'== fLit -> return fLit -- do not evaluate right operand
                    OR  | e1'== tLit -> return tLit -- ''
                    _                -> eval e2 >>= calc op e1'
                
        LitExpr lit -> return lit
    
        IdExpr idf -> getFromMap (name idf) Nothing
            
        Prefixed op e -> case op of
            Incr    -> incdec e Addition False
            Decr    -> incdec e Subtraction False     
            Not     -> eval e >>= unaryb2b not
            Minus   -> eval e >>= \l -> if isInt l then unaryi2i negate l else unaryd2d negate l
            Plus    -> eval e >>= \l -> if isInt l then unaryi2i id l else unaryd2d id l
            
        Postfixed op e@(IdExpr _) -> case op of
            Incr    -> incdec e Addition True
            Decr    -> incdec e Subtraction True
            _       -> throwEvalError "Parse error, should not happen"
        
        -- TODO aanpassen
        Call (Identifier i) params -> case i of
            -- TODO mag ook met meerdere args in PHP
            "max"   -> 
                do
                    checkParams params 2 
                    param1 <- eval $ params!!0
                    param2 <- eval $ params!!1
                    binaryi2i (return max) (toInt param1) (toInt param2) 
            
            "str_repeat" ->        
                do 
                    -- Returns input repeated multiplier times.
                    -- http://php.net/manual/en/function.str-repeat.php 
                    checkParams params 2
                    inpt <- eval $ params!!0
                    multiplier <- eval $ params!!1
                    res <- replicate <$> toInt multiplier <*> toString inpt
                    return $ StringLiteral $ concat res
            "rand" ->
                do
                    -- TODO real random numbers
                    return $ IntLiteral 13
            "pow" ->
                do     
                    checkParams params 2 
                    param1 <- eval $ params!!0
                    param2 <- eval $ params!!1
                    binaryi2i (return (^)) (toInt param1) (toInt param2) 
            "print" ->
                do
                    checkParams params 1 
                    param1 <- eval $ params!!0
                    addToOutput param1
                    return Null

            _ ->
                do
                    -- To Do, params
                    p <- retrieveProgram
                    let method = findMethodByName (Identifier i) p
                    when (isNothing method) $ throwEvalError "Method not found"
                    when (isJust $ findMethod p) $ throwEvalError "Recursion not supported"
                    retVal <- eval (fromJust method)
                    return $ fromMaybe Null retVal

            _       -> throwEvalError $ "Unknown function call " ++ show i
                
        ArrayAcc idf index -> 
            do
                idx <- eval index >>= toInt
                getFromMap (name idf) (Just idx)
        
        Property i pro -> case name pro of
            "length" -> 
                do
                    array <- getFromMap (name i) Nothing
                    case array of
                        (ArrayLit a) -> return $ IntLiteral (S.length a)
                        _            -> throwEvalError "length not defined"
                        
            _ -> throwEvalError "Unknown property"
           
        NewArray dt size -> 
            do
                size' <- eval size >>= toInt
                return $ ArrayLit $ S.replicate size' $ defaultValue dt
      
        x -> throwError (NotSupported $ show x)
        
        where
            incdec e@(IdExpr idf) preOp isPostfix = 
                do             
                    e' <- eval e
                    result <- calc preOp e' (IntLiteral 1)
                    addToMap (name idf) Nothing result
                    return $ if isPostfix then e' else result
            incdec _ _ _ = throwEvalError "Invalid use of ++ or --" 
            
            checkParams params expected = 
                when (length params /= expected) $ throwEvalError "Wrong number of function params" 

--------------------------------------------------------------------------------
-- Method evaluator

instance Evaluation BClassMember (Maybe Literal) where
    eval cm = case cm of
        Attribute {} -> throwError (NotSupported $ show cm)

        Constructor {} -> throwError (NotSupported $ show cm)
        
        BMethod { mParams = params, mBody = bdy } -> 
            do
                -- store the program in the state
                modify (\s -> s { program = Just (B.OOProgram [B.Class (makeIdentifier "Temp") [cm]]) }) 

                inParams <- gets input
                -- check types
                unless (typeMatch inParams params) $ throwError (EvalError "Test cases do not match the parameters")
                -- store input values
                zipWithM_ (\p pv -> addToMap (getName p) Nothing pv) params inParams
                eval bdy
                return Nothing -- void
                `catchError` (\err -> 
                                case err of 
                                    ReturnEncountered val -> return val
                                    _                     -> throwError err
                            )
        where
            typeMatch :: [Literal] -> [Param] -> Bool
            typeMatch inValues inParams = length inParams == length inValues
                            && and (zipWith (\p v -> isNothing (litType v) || paramType p == fromJust (litType v)) inParams inValues)
                            -- TODO better array checks, check entire test set?

instance Evaluation BFragment () where
    eval = eval . bstats
--------------------------------------------------------------------------------
-- Helpers 

throwEvalError :: MonadError EvalResult m => String -> m a
throwEvalError s = throwError (EvalError s)

retrieveProgram :: Eval BProgram
retrieveProgram = do
    maybeP <- gets program
    when (isNothing maybeP) $ throwEvalError "Program not stored in state"
    return (fromJust maybeP)

addToOutput :: ToOutput a => a -> Eval ()
addToOutput out = modify (\state -> state { output = output state ++ [toOutput out] }) 

-- inserts the value of a variable into a map, possibly overwriting the existing value
addToMap :: VarName -> Maybe Int -> Literal -> Eval ()
addToMap var idx val = do
    newVal <- case idx of
        Nothing -> return val
        Just idx' -> do
            oldArray <- getFromMap var Nothing
            _ <- getArrayVal var idx' -- checks for valid index
            return $ ArrayLit $ S.update idx' val (fromJust $ getArray oldArray)
    modify (\state -> state { environment = Map.insert var newVal (environment state) })
             
-- returns the value of an array at an index or fails                    
getArrayVal :: VarName -> Int -> Eval Literal
getArrayVal var idx = do
    val <- getValue var 
    array <- case val of 
        ArrayLit x -> return x
        _          -> throwEvalError $ var ++ " is no array"
    when (S.length array <= idx || idx < 0) $ throwEvalError "Index out of bounds"
    return $ S.index array idx 

getFromMap :: VarName -> Maybe Int -> Eval Literal
getFromMap var = maybe (getValue var) (getArrayVal var) 

getValue :: VarName -> Eval Literal
getValue var = fmap (Map.lookup var) (gets environment) 
    >>= maybe (throwEvalError $ var ++ " not found in map") return  
                            
calc :: (MonadError EvalResult m, Applicative m) => InfixOp -> Literal -> Literal -> m Literal
calc op lt1 lt2
    | op == Addition && (isString lt1 || isString lt2)
                                             = return $ concat2s lt1 lt2

    | op == Division && (lt2 == iLit0 || lt2 == dLit0)   = throwEvalError "Division by zero" 
    | op == Remainder && lt2 == iLit0                   = throwEvalError "Division by zero"  
    | isArithmeticOp op && isInt lt1 && isInt lt2 = binaryi2i (toNumInfixOp op) (toInt lt1) (toInt lt2)
    | isArithmeticOp op                     = binaryd2d (toNumInfixOpD op) (toDouble lt1) (toDouble lt2)

    | isBoolOp op                           = binary2b (toBoolOp op) (toBool lt1) (toBool lt2)

    | isComparisonOp op && isInt lt1 && isInt lt2    = binary2b (toCompOp op) (toInt lt1) (toInt lt2)
    | isComparisonOp op && (isDouble lt1 || isDouble lt2) = binary2b (toCompOp op) (toDouble lt1) (toDouble lt2)

    | isComparisonOp op && isString lt1     = binary2b (toCompOp op) (toString lt1) (toString lt2)
    | isComparisonOp op && isBool lt1       = binary2b (toCompOp op) (toBool lt1) (toBool lt2)

    -- TODO compare doubles
    | isBitwiseOp op && isInt lt1 && isInt lt2 = binaryi2i (toBitwiseOp op) (toInt lt1) (toInt lt2)
    | otherwise                             = throwEvalError "Invalid calculation"

binaryd2d :: (Monad m, Applicative m, Fractional a, Fractional b) => m (a -> b -> Double) -> m a -> m b -> m Literal
binaryd2d f a b = DoubleLiteral <$> (f <*> a <*> b) 

toInt :: (MonadError EvalResult m) => Literal -> m Int
toInt (IntLiteral i)        = return i
toInt _                     = throwEvalError "Int expected"

toDouble :: (MonadError EvalResult m) => Literal -> m Double
toDouble (DoubleLiteral d)     = return d
toDouble (IntLiteral i)        = return (fromIntegral i)
toDouble _                     = throwEvalError "Double expected"

toString :: (MonadError EvalResult m) => Literal -> m String      
toString (StringLiteral s)  = return s
toString _                  = throwEvalError "String expected"

toBool :: (MonadError EvalResult m) => Literal -> m Bool
toBool (BoolLiteral b)      = return b
toBool _                    = throwEvalError "Bool expected"
             
unaryb2b :: (MonadError EvalResult m) => (Bool -> Bool) -> Literal -> m Literal
unaryb2b f (BoolLiteral b)  = return $ BoolLiteral $ f b
unaryb2b _ _                = throwEvalError "Bool expected"

unaryi2i :: (MonadError EvalResult m) => (Int -> Int) -> Literal -> m Literal
unaryi2i f (IntLiteral b) = return $ IntLiteral $ f b
unaryi2i _ _ = throwEvalError "Integer expected"

unaryd2d :: (MonadError EvalResult m) => (Double -> Double) -> Literal -> m Literal
unaryd2d f (DoubleLiteral d) = return $ DoubleLiteral $ f d
unaryd2d _ _ = throwEvalError "Double expected"

binary2b :: (Monad m, Applicative m) => m (a -> a -> Bool) -> m a -> m a -> m Literal
binary2b f a b = BoolLiteral <$> (f <*> a <*> b) 

binaryi2i :: (Monad m, Applicative m) => m (Int -> Int -> Int) -> m Int -> m Int -> m Literal
binaryi2i f a b = IntLiteral <$> (f <*> a <*> b) 

concat2s :: Literal -> Literal -> Literal
concat2s a b = StringLiteral $ toOutput a ++ toOutput b

toNumInfixOp :: MonadError EvalResult m => Integral a => InfixOp -> m (a -> a -> a)
toNumInfixOp Addition       = return (+)
toNumInfixOp Subtraction    = return (-)
toNumInfixOp Multiplication = return (*)
toNumInfixOp Division       = return div 
toNumInfixOp Remainder      = return rem
toNumInfixOp _              = throwEvalError "No numerical operator"

toNumInfixOpD :: MonadError EvalResult m => Fractional a => InfixOp -> m (a -> a -> a)
toNumInfixOpD Addition       = return (+)
toNumInfixOpD Subtraction    = return (-)
toNumInfixOpD Multiplication = return (*)
toNumInfixOpD Division       = return (/) 
toNumInfixOpD _              = throwEvalError "Operator not supported for doubles"

toCompOp :: MonadError EvalResult m => Ord a => InfixOp -> m (a -> a -> Bool)
toCompOp Equal              = return (==)
toCompOp NotEqual           = return (/=)
toCompOp Greater            = return (>)
toCompOp GreaterOrEqual     = return (>=)
toCompOp Less               = return (<)
toCompOp LessOrEqual        = return (<=)
toCompOp _                  = throwEvalError "No comparison operator"

toBoolOp :: MonadError EvalResult m => InfixOp -> m (Bool -> Bool -> Bool)
toBoolOp AND                = return (&&)
toBoolOp OR                 = return (||)
toBoolOp _                  = throwEvalError "No boolean operator"

toBitwiseOp :: MonadError EvalResult m => Integral a => Data.Bits.Bits a => InfixOp -> m (a -> a -> a)
toBitwiseOp BAnd = return (.&.)
