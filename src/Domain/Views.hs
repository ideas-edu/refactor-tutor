{-# LANGUAGE LambdaCase #-}
module Domain.Views where

import Ideas.Common.View
import Domain.Syntax
import Utils.Utils
import Control.Applicative
import Control.Monad
import Data.Generics.Uniplate.DataOnly (universeBi)
import Data.List
import Data.Maybe


-- TODO pattern synonyms
pattern Assign a = Assignment Assign 

matchId :: Matcher a a
matchId = makeMatcher Just

-- Statements

matchSingleBlock :: Matcher Statement Statement
matchSingleBlock = matchSingleBlockWith matchId matchId

matchSingleBlockWith :: IsMatcher m => Matcher Statement Statement -> m Statement a -> Matcher Statement a
matchSingleBlockWith mClean mS = makeMatcher $ \s -> do
    s' <- match mClean s
    case s' of
        Block _ [s] -> match mS s
        Block _ _   -> Nothing
        s           -> match mS s

matchStatListTop :: (IsMatcher m) => m [Statement] a -> Matcher Statement a
matchStatListTop m = makeMatcher (match m . getBlockStats)

matchLast2F :: Matcher Statement a -> Matcher Statement b -> Matcher Fragment ([Statement], a, b)
matchLast2F m1 m2 = makeMatcher $ \f -> match (matchLast2 m1 m2) (stats f)

-- Extract the single statement that matches the arg
matchSingleStatTop :: IsMatcher f => f Statement a
     -> Matcher Statement ([Statement], a, [Statement])
matchSingleStatTop mFind = makeMatcher $ \s -> do
    topList <- match (matchStatListTop matchId) s
    let findMs = findIndices (isJust . match mFind) topList -- where does it match?
        idx    = head findMs
    guard (length findMs == 1)
    res <- match mFind (topList !! idx)
    return (take idx topList, res, drop (1+idx) topList)

matchBreak :: Matcher Statement Statement
matchBreak = makeMatcher $ \case
    Break -> Just Break
    _     -> Nothing

-- 
matchStatList :: Matcher Statement [Statement]
matchStatList = makeMatcher $ Just . universeBi

-- [return t/f]
matchSingleBoolReturn :: Matcher Statement Bool
matchSingleBoolReturn = matchSingleBlockWith matchId matchAsBoolReturn

-- If-statements

matchIfsWith :: (IsMatcher a, IsMatcher b, IsMatcher c) =>
     a Expression d -> b Statement e -> c Statement f -> Matcher Statement (d, e, Maybe f)
matchIfsWith mC mT mF = makeMatcher $ \s -> case s of
    IfElse {} -> (\(c', t', f') -> (c', t', Just f')) <$> match (matchIfElseWith mC mT mF) s
    If {}     -> (\(c', t')     -> (c', t', Nothing)) <$> match (matchIfWith mC mT) s
    _         -> Nothing


viewIfs :: View Statement (Expression, Statement, Maybe Statement)
viewIfs = makeView (match (matchIfsWith matchId matchId matchId)) b
    where
        b (c, t, Nothing) = If c t
        b (c, t, Just f)  = IfElse c t f

matchIf :: Matcher Statement (Expression, Statement)
matchIf = matchIfWith matchId matchId

matchIfWith :: (IsMatcher a, IsMatcher b) => a Expression d -> b Statement e -> Matcher Statement (d, e)
matchIfWith mC mT = makeMatcher $ \case 
    (If c t)       -> toTuple2 <$> match mC c <*> match mT t
    _              -> Nothing

viewIfElse :: View Statement (Expression, Statement, Statement)
viewIfElse = matcherView matchIfElse $ \(c, t, f) -> IfElse c t f

viewIfElseWith :: (IsMatcher a, IsMatcher b, IsMatcher c) =>
    a Expression Expression -> b Statement Statement -> c Statement Statement -> View Statement (Expression, Statement, Statement)
viewIfElseWith vC vT vF = matcherView (matchIfElseWith vC vT vF) $ \(c, t, f) -> IfElse c t f

matchIfElse :: Matcher Statement (Expression, Statement, Statement)
matchIfElse = matchIfElseWith matchId matchId matchId

matchIfElseWith :: (IsMatcher a, IsMatcher b, IsMatcher c) =>
     a Expression d -> b Statement e -> c Statement f -> Matcher Statement (d, e, f)
matchIfElseWith mC mT mF = makeMatcher $ \case 
    (IfElse c t f) -> toTuple3 <$> match mC c <*> match mT t <*> match mF f
    _              -> Nothing

matchReturn :: Matcher Expression a -> Matcher Statement a
matchReturn mReturn = makeMatcher $ \case
    Return (Just r) -> match mReturn r
    _               -> Nothing

matchAsBoolReturn :: Matcher Statement Bool
matchAsBoolReturn = matchReturn matchBoolLit

matchAsIdtReturn :: Matcher Statement Identifier
matchAsIdtReturn = matchReturn matchIdt

matchBoolAssign :: Matcher Statement (Identifier, Bool)
matchBoolAssign = matchAssign matchBoolLit

matchAssign :: Matcher Expression a -> Matcher Statement (Identifier, a)
matchAssign mVal = matchAssignWith matchIdt mVal

matchAssignWith :: (IsMatcher a, IsMatcher b) => a Expression p -> b Expression q -> Matcher Statement (p, q)
matchAssignWith mL mR = makeMatcher $ \case
    ExprStat (Assignment Assign l r) -> toTuple2 <$> match mL l <*> match mR r
    _                                -> Nothing

-- Expressions
matchBoolLit :: Matcher Expression Bool
matchBoolLit = makeMatcher $ \case
    LiteralExpr (BoolLiteral b) -> Just b
    _                           -> Nothing

matchTrue :: Matcher Expression ()
matchTrue = makeMatcher $ \e -> match matchBoolLit e >>= guard 

matchFalse :: Matcher Expression ()
matchFalse = makeMatcher $ \e -> match matchBoolLit e >>= guard . not

matchIntLit :: Matcher Expression Int
matchIntLit = makeMatcher $ \case
    LiteralExpr (IntLiteral i) -> Just i
    _                          -> Nothing

matchIdt :: Matcher Expression Identifier
matchIdt = makeMatcher $ \case
    (IdExpr idt) -> Just idt
    _            -> Nothing

matchIdtWith :: Identifier -> Matcher Expression ()
matchIdtWith idt = makeMatcher $ \e -> match matchIdt e >>= guard . (== idt)

matchNotIdt :: Matcher Expression Identifier
matchNotIdt = makeMatcher $ \case
    (Prefixed Not (IdExpr idt)) -> Just idt
    (Infixed Equal (IdExpr idt) b) -> match matchFalse b >> Just idt --TODO use norms.
    _                           -> Nothing

-- for ( ..; ..; x++
-- TODO: abstraction for commutativity etc
viewIncrOne :: View Expression Identifier
viewIncrOne = makeView m (Postfixed Incr . IdExpr)
    where 
        m (Postfixed Incr (IdExpr idt))         = Just idt
        m (Prefixed Incr (IdExpr idt))          = Just idt
        m (Assignment AssignAdd (IdExpr idt) 1) = Just idt
        m (Assignment Assign (IdExpr idt) (Infixed Addition (IdExpr idt') 1)) | idt == idt' = Just idt
        m (Assignment Assign (IdExpr idt) (Infixed Addition 1 (IdExpr idt'))) | idt == idt' = Just idt
        m _ = Nothing

viewDecrWithN :: View Expression (Identifier, Int)
viewDecrWithN = makeView m (\(x, n) -> Assignment AssignSub (IdExpr x) (makeInt n))
    where 
        m (Postfixed Decr (IdExpr idt))         = Just (idt, 1)
        m (Prefixed Decr (IdExpr idt))          = Just (idt, 1)
        m (Assignment AssignSub idt n) = toTuple2 <$> match matchIdt idt <*> match matchIntLit n
        m (Assignment Assign l r )  = do
            idtL <- match matchIdt l
            (r', n) <- match (matchInfix Subtraction) r
            n' <- match matchIntLit n
            idtR <- match matchIdt r'
            guard (idtL == idtR)
            Just (idtL, n')
        -- m (Assignment Assign (IdExpr idt) (Infixed Subtraction n (IdExpr idt'))) | idt == idt' = Just (idt, n)
        m _ = Nothing

-- TODO with 2 matchers
matchInfix :: InfixOp -> Matcher Expression (Expression, Expression)
matchInfix op = makeMatcher $ \case
    Infixed op1 l r | op1 == op -> Just (l, r)
    _                           -> Nothing


-- Loops ----------------------------------------------------------------------    

matchFor :: Matcher Statement (ForInit, [Expression], [Expression], Statement)
matchFor = matchForWith matchId matchId matchId matchId

matchForWith :: (IsMatcher a, IsMatcher b, IsMatcher c, IsMatcher d) =>
    a ForInit p -> b [Expression] q -> c [Expression] r -> d Statement s 
        -> Matcher Statement (p, q, r, s)
matchForWith mA mB mC mBody = makeMatcher $ \case 
    (For a b c body) -> toTuple4 <$> match mA a <*> match mB b <*> match mC c <*> match mBody body
    _                -> Nothing

matchForWithBody :: IsMatcher m => m Statement s -> Matcher Statement (ForInit, [Expression], [Expression], s)
matchForWithBody = matchForWith matchId matchId matchId 

matchWhile :: (IsMatcher a, IsMatcher b) =>
    a Expression p -> b Statement q -> Matcher Statement (p, q)
matchWhile mC mBody = makeMatcher $ \case 
    (While c body) -> toTuple2 <$> match mC c <*> match mBody body
    _              -> Nothing

type Loop = (ForInit, Expression, [Expression], Statement)

-- Extract condition and body of for & while 
-- TODO hergebruik?
-- TODO niet mooi
matchLoop :: (IsMatcher a, IsMatcher b, IsMatcher c, IsMatcher d) =>
    a ForInit ForInit -> b Expression Expression -> c [Expression] [Expression] -> d Statement Statement ->  Matcher Statement (Loop, Statement)
matchLoop mA mB mC mBody =  makeMatcher $ \s -> fmap (\l -> (l, s)) $
        match (matchForWith mA (matchList1 mB) mC mBody) s 
    <|> fmap (\(c, b) -> (emptyForInit, c, [], b)) (match (matchWhile mB mBody) s)

viewLoop :: View Statement (Loop, Statement)
viewLoop = viewLoopWith matchId matchId

viewLoopWith :: (IsMatcher a, IsMatcher b) =>
     a Expression Expression -> b Statement Statement -> View Statement (Loop, Statement)
viewLoopWith vC vBody = matcherView (matchLoop matchId vC matchId vBody) b
    where
        b ((p, q, r, s), For {})   = For p [q] r s
        b ((_, q, _, s), While {}) = While q s 
        b _                        = error "viewLoop"

matchLoopZeroToN :: IsMatcher a => a Statement s
    -> Matcher Statement (Identifier, (Identifier, Identifier), Identifier, s)
matchLoopZeroToN mBody = makeMatcher $ \s -> do
    l@(a, (b, n), c, d) <- match (matchForWith viewInitZero (matchList1 viewToN) (matchList1 viewIncrOne) mBody) s
    guard (a == b && b == c)
    return l

-- for ( ; x < n; ..
viewToN :: View Expression (Identifier, Identifier)
viewToN = viewXToN matchId matchId

-- x < n
viewXToN :: (IsMatcher a, IsMatcher b) => a Identifier Identifier -> b Identifier Identifier
      -> View Expression (Identifier, Identifier)
viewXToN vX vN = makeView m b
    where 
        b (x, n) = IdExpr x .<. IdExpr n
        m (Infixed Less (IdExpr x) (IdExpr n))     = toTuple2 <$> match vX x <*> match vN n
        m (Infixed Greater (IdExpr n) (IdExpr x))  = toTuple2 <$> match vX x <*> match vN n
        m _                                    = Nothing

-- for ( ; x < y.length; ..
viewToArrLength :: View Expression (Identifier, Identifier)
viewToArrLength = makeView m b
    where 
        b (aI, cI) = IdExpr cI .<. Property aI (Identifier "length")
        m (Infixed Less (IdExpr cI) (Property aI (Identifier "length")))      = Just (cI, aI)
        m (Infixed Greater (Property aI (Identifier "length")) (IdExpr cI) )  = Just (cI, aI)
        m _ = Nothing

-- for ( [int] x = 0;..
viewInitZero :: View ForInit Identifier
viewInitZero = makeView m b
    where
        b cI = ForInitExpr [IdExpr cI .=. lit0]
        m (ForInitExpr [Assignment Assign (IdExpr c) 0])          = Just c
        m (ForInitDecls IntType [Assignment Assign (IdExpr c) 0]) = Just c
        m _ = Nothing

-- Generic matchers

matchList1 :: (IsMatcher m) => m a b -> Matcher [a] b
matchList1 m = makeMatcher $ \case
    [x]   -> match m x
    _     -> Nothing

matchLast2 :: (IsMatcher m, IsMatcher n) => m a b -> n a c -> Matcher [a] ([a], b, c)
matchLast2 m1 m2 = makeMatcher $ \xs -> 
    case splitAt (length xs - 2) xs of
        (pre, [x,y]) -> (\x' y' -> (pre, x', y')) <$> match m1 x <*> match m2 y
        _            -> Nothing

matchLast :: Matcher [a] ([a], a)
matchLast = matchLastWith matchId

matchLastWith :: (IsMatcher m) => m a z -> Matcher [a] ([a], z)
matchLastWith m = makeMatcher $ \case
    [] -> Nothing
    xs -> (\z -> (init xs, z)) <$> match m (last xs)





{- type View a b = (a -> Maybe b, b -> a) 
type Lens a b = a -> Maybe (b, b -> a)

lensIf (IfElse c t f) = Just ((c, t, f), \(c, t, f) -> IfElse c t f)
lensIf (If c t) = Just ((c, t, Empty), \(c, t, f) -> if f==Empty then If c t else ...)


type Lens a b = a -> Maybe (b, b -> a)

lensLoop :: Lens Statement (Expression, Statement)
lensLoop (While c b)   = Just ( (c, b), \(c, b) -> While c b)
lensLoop (For a b c d) = Just ( ()  )-}