------------------------------------------------------------------
-- 
--
-- Created: 10-3-2018
--
------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

module Domain.Refactoring.Rules.Rules where

import Domain.Refactoring.Util
import Domain.Printer
import Domain.Syntax hiding ((./.))
import Domain.Views
import Ideas.Common.Library as Ideas hiding ((.%.))
import Ideas.Common.Rewriting.Confluence
import Data.Maybe
import Control.Applicative hiding (many)
import Control.Monad
import Domain.Terms


exAsStatView :: View Statement Expression
exAsStatView = makeView sToE ExprStat
    where
        sToE (ExprStat e) = Just e
        sToE _            = Nothing

------------------------------------------------------------
-- Rule lists


ifRRules :: [RewriteRule Statement]
ifRRules = [reverseNegIfElse, removeRedundantIfElse, removeEmptyElse, removeEmptyIf] 
    ++ removeUselessIf ++ removeUselessIfElse ++ removeUselessCheck ++ collapseIf ++ simplifyIfs

blockRRules :: [RewriteRule Statement]
blockRRules = [removeEmptyBlock, removeSingleBlock] 

blockRules :: [Rule Statement]
blockRules = [removeEmptyStats]

checkConf :: IO ()
checkConf = do
    putStrLn "=== ExprRules ==="
    --checkConfluenceWith cfg exprRRules
    putStrLn "=== IfRules ==="
    --checkConfluenceWith cfg ifRules
    putStrLn "=== BlockRules ==="
    --checkConfluenceWith cfg blockRules
    putStrLn "=== CleanRules ==="
    --checkConfluenceWith cfg cleanRRules
    putStrLn "=== mCleanRules ==="
    --checkConfluenceWith cfg minorCleanRRules
    putStrLn "=== CollRules ==="
    --checkConfluenceWith cfg collapseIf

    where
        cfg = defaultConfig { showTerm = ("\n" ++ ) . fromJust . toS }

        toS :: Term -> Maybe String
        toS t = 
                fmap showPretty (fromTerm t :: Maybe Statement) 
            <|> fmap showPretty (fromTerm t :: Maybe Expression) 
            <|> Just "?"

instance LiftView RewriteRule where
    -- View a (b, c) -> RewriteRule b -> RewriteRule a
   liftViewIn v r = error "liftViewIn" -- undefined

------------------------------------------------------------
-- Rules on statements

-- If-else statements --------------------------------------

checkIfElse :: Strategy (Context Statement)
checkIfElse = check (maybe False isIfElse . currentInContext)

checkIf :: Strategy (Context Statement)
checkIf = check (maybe False isIf . currentInContext)

conditionalS :: LabeledStrategy (Context Statement)
conditionalS = label "conditionalS" $
        useC checkIfElse
    .*. (       
                (removeEmptyElseS ./. removeEmptyIfS)
            |> choice (useRs simplifyIfs) 
            |> useR reverseNegIfElse
        )

reverseNegIfElse :: RewriteRule Statement
reverseNegIfElse = makeRewriteRule "reverseNegIfElse" $
    \c t f -> IfElse (nt c) t f :~> IfElse c f t

removeEmptyElseS :: Strategy (Context Statement)
removeEmptyElseS = withElsePart (try $ useMR removeEmptyBlock) -- for empty braces
     .*. useR removeEmptyElse
  
removeEmptyElse :: RewriteRule Statement
removeEmptyElse = makeRewriteRule "removeEmptyElse" $
    \c t -> IfElse c t Empty :~> If c t

removeEmptyIf :: RewriteRule Statement
removeEmptyIf = makeRewriteRule "removeEmptyIf" $ 
    \c f -> IfElse c Empty f :~> If (nt c) f

-- in steps
removeEmptyIfS :: LabeledStrategy (Context Statement)
removeEmptyIfS = label "removeEmptyIfS" $ 
        -- Change this check!!
        check (\c -> case currentInContext c of
                        Just (IfElse _ Empty _) -> True
                        Just (IfElse _ (Block _ []) _) -> True
                        _                       -> False)
    .*. (useR reverseNegIfElse |> useR reverseIfElse )
    .*. removeEmptyElseS

reverseIfElse :: RewriteRule Statement
reverseIfElse = makeRewriteRule "reverseIfElse" $
    \c t f -> IfElse c t f :~> IfElse (nt c) f t

-- >
tryRemoveEmptyStatsMinor :: Strategy (Context Program)
tryRemoveEmptyStatsMinor = use (minor removeEmptyStats) .|. use (minor removeEmptyStatsL)

removeUselessIfS :: LabeledStrategy (Context Program)
removeUselessIfS = label "removeUselessIfS" $ somewhere $
        useC checkIf
    .*. withIfPart (try $ useMR removeEmptyBlock) .*. orelse (useRs removeUselessIf)
    .*. try (ruleUp .*. tryRemoveEmptyStatsMinor)

removeUselessIf :: [RewriteRule Statement]
removeUselessIf = makeRewriteRules "removeUselessIf" 
    [ \c _ -> If c Empty            :~> Empty
    , \_ a -> If trueLt a           :~> a
    , \_ a -> If falseLt a          :~> Empty
    ]

removeUselessIfElseS :: LabeledStrategy (Context Program)
removeUselessIfElseS = label "removeUselessIfElseS" $ -- somewhere $
        useC checkIfElse
    .*. withIfPart (try $ useMR removeEmptyBlock) .*. withElsePart (try $ useMR removeEmptyBlock)
    .*. orelse (useRs removeUselessIfElse)
    .*. try (ruleUp .*. tryRemoveEmptyStatsMinor)

removeUselessIfElse :: [RewriteRule Statement]
removeUselessIfElse = makeRewriteRules "removeUselessIfElse" 
    [ \c _ _ -> IfElse c Empty Empty  :~> Empty
    , \c a b -> IfElse falseLt a b    :~> b
    , \c a b -> IfElse trueLt a b     :~> a
    ]

-- TODO
removeRedundantIfElse :: RewriteRule Statement
removeRedundantIfElse = makeRewriteRule "removeRedundantIfElse" $ 
    \c t -> IfElse c t t :~> t

-- inverse of a simplify rule, also for BStat
expandIfWithOr :: [RewriteRule Statement]
expandIfWithOr = makeRewriteRules "expandIfWithOr" 
    [ \p q t _ -> If     (p .||. q) t   :~> IfElse p t (If q t)
    , \p q t f -> IfElse (p .||. q) t f :~> IfElse p t (IfElse q t f)
    ]

-- remove negated check in else
removeUselessCheckS :: LabeledStrategy (Context Statement)
removeUselessCheckS = label "removeUselessCheckS" $ 
        useC checkIfElse
     .*. withElsePart (try (useMR removeSingleBlock)) .*. withIfCondition (try . choice $ map useMR extractNot)
     .*. choice (useRs removeUselessCheck)

-- TODO remove last 2, view in removeUselessCheckS
removeUselessCheck :: [RewriteRule Statement]
removeUselessCheck = makeRewriteRules "removeUselessCheck" 
    [ \p a b _ -> IfElse p a (If (nt p) b) :~> IfElse p a b
    , \p a b c -> IfElse p a (IfElse (nt p) b c) :~> IfElse p a b
    , \p a b c -> IfElse (nt p) a (If p b) :~> IfElse (nt p) a b
    , \p a b c -> IfElse (nt p) a (IfElse p b c) :~> IfElse (nt p) a b
    ]

collapseIf :: [RewriteRule Statement]
collapseIf = makeRewriteRules "collapseIf"
    [ \c1 c2 t _ -> If c1 (If c2 t)          :~> If (c1 .&&. c2) t
    , \c1 c2 t f -> IfElse c1 (If c2 t) f    :~> IfElse (c1 .&&. c2) t (If (nt c1) f)
    ]

buggyCollapseIfR :: Rule Statement
buggyCollapseIfR = buggy $ makeRule "buggyCollapseIf" $ \s -> do
    -- TODO eerst norm.
    -- use $ withIfPart (try (useMR removeSingleBlock)) .*. 
    let bciS = choice buggyCollapseIf
    apply bciS s


buggyCollapseIf :: [Rule Statement]
buggyCollapseIf = map (buggy . ruleRewrite) . makeRewriteRules "buggyCollapseIf" $
    [ \c1 c2 t _ -> If c1 (If c2 t)          :~> If (c1 .||. c2) t
    , \c1 c2 t f -> IfElse c1 (If c2 t) f    :~> IfElse (c1 .&&. c2) t f
    , \c1 c2 t f -> IfElse c1 (Block 0 [If c2 t]) f    :~> IfElse (c1 .&&. c2) t f
    ]

-- Nested if/ifelse that can be simplified
simplifyIfWithDuplicationS :: LabeledStrategy (Context Statement)
simplifyIfWithDuplicationS = label "simplifyIfWithDuplicationS" $ 
        useC checkIfElse
    .*. innermost (useMR removeSingleBlock) -- full/once werkt niet volledig, somewhere te veel derivs
    -- innenr/outer maakt niet uit?
    -- .*.     many (somewhere$ useMR removeSingleBlock)
    -- TODO more normalising
    .*. choice (useRs simplifyIfs)


-- if c1
--     p1
-- else
--     if c2
--         p1
------------------
-- if p
--     a
-- else
--     if q
--         b
--     else
--         a   
------------------
-- if p
--     a
-- else
--     if q
--         a
--     else
--         b   
simplifyIfs :: [RewriteRule Statement]
simplifyIfs = makeRewriteRules "simplifyIf"
    [ \p a q _ -> IfElse p a (If q a)       :~> If (p .||. q) a
    , \p a q b -> IfElse p a (IfElse q b a) :~> IfElse (nt p .&&. q) b a 
    , \p a q b -> IfElse p a (IfElse q a b) :~> IfElse (p .||. q) a b
    -- , \p a q b -> IfElse p (If q a) a       :~> If (nt p .||. q) a 
    , \p a q _ -> IfElse p (If q a) a       :~> If ((p .&&. q) .||. nt p) a --ex1
    ]

-- Build an extra layer for layered feedback
extractFromIfS :: LabeledStrategy Statement
extractFromIfS = label "extractFromIfS" extractFromIf

-- Simplification, only last statement is equal
extractFromIf :: Rule Statement
extractFromIf = makeRule "extractFromIf" $ \s -> do
    (c, (pT, lT), (pF, lF)) <- match (matchIfElseWith matchId (matchStatListTop matchLast) (matchStatListTop matchLast)) s
    guard (lT == lF)
    return $ makeBlock [IfElse c (makeBlock pT) (makeBlock pF)
                       , lT]

-- Blocks --------------------------------------------------

-- removing unneeded blocks and empty stats
cleanStructure :: (IsProgram p, IsTerm p) => Strategy (Context p)
cleanStructure = outermost (
    minor (useR removeSingleBlock) |> useR removeEmptyBlock |> use removeEmptyStats)

-- removing unneeded blocks and empty stats
cleanStructureM :: (IsProgram p, IsTerm p) => Strategy (Context p)
cleanStructureM = outermost (
    minor (useR removeSingleBlock) |> minor(useR removeEmptyBlock) |> minor(use removeEmptyStats))

cleanStructureTop :: Strategy Statement
cleanStructureTop = try removeEmptyStats .*. try (removeSingleBlock .|. removeEmptyBlock)

removeEmptyBlock :: RewriteRule Statement
removeEmptyBlock = makeRewriteRule "removeEmptyBlock" $
    \loc -> Block loc [] :~> Empty

-- Also at top level?
removeSingleBlock :: RewriteRule Statement
removeSingleBlock = makeRewriteRule "removeSingleBlock" $
    \loc s -> Block loc [s] :~> s

-- | Removes empty statements only from blocks
removeEmptyStats :: Rule Statement
removeEmptyStats = makeRule "removeEmptyStats" (\case
    Block i xs -> fmap (Block i) (removeEmptyStats' xs)
    _          -> Nothing
    )

-- | Removes empty statements only from fragments
removeEmptyStatsF :: Rule Fragment
removeEmptyStatsF = makeRule "removeEmptyStatsF" (\f ->
        fmap (\newS -> f { stats = newS } ) (removeEmptyStats' $ stats f)
    )

-- | Removes empty statements only lists of statements
removeEmptyStatsL :: Rule [Statement]
removeEmptyStatsL = makeRule "removeEmptyStatsL" removeEmptyStats'

removeEmptyStats' :: [Statement] -> Maybe [Statement]
removeEmptyStats' xs =
    if Empty `elem` xs 
        then Just $ filter (/= Empty) xs
        else Nothing

-- Variables -----------------------------------------------

removeAssignToSelfS :: LabeledStrategy (Context Fragment)
removeAssignToSelfS = label "removeAssignToSelfS" $ somewhere $
    useR removeAssignToSelf .*. ruleUp .*. try (use (minor removeEmptyStats) .|. use (minor removeEmptyStatsL)) 
    -- no ruleDown, because list might become empty

removeAssignToSelf :: RewriteRule Statement
removeAssignToSelf = makeRewriteRule "removeAssignToSelf" $   
    \l -> ExprStat (l .=. l) :~> Empty

--removeUnusedVarsR :: Rule Fragment
--removeUnusedVarsR = makeRule "" $ \f -> undefined

-- TODO finish
combineInitDecl :: Rule Statement
combineInitDecl = makeRule "combineInitDecl" (\s ->
    case s of
        -- Block i s -> if 
        _         -> Nothing
        )
-- \dt idt val -> [VarDeclarations dt [IdExpr idt], ExprStat ((IdExpr idt) .=. val)] :~> [VarDeclarations dt [(IdExpr idt) .=. val]]

initDecl = initDecl' 0

initDecl' idx ((VarDeclarations dt [IdExpr idt1]) : (ExprStat (Assignment Assign (IdExpr idt2) val )) : _) = Just idx
initDecl' idx (_:y:z:xs) = initDecl' (idx + 1) (y:z:xs)
initDecl' _ _ = Nothing

-- Returns -------------------------------------------------  

normIfS :: Strategy (Context Statement)
normIfS = checkC isIf .*. withIfPart cleanStructureM

normIfElseS :: Strategy (Context Statement)
normIfElseS = checkC isIfElse .*. withIfPart cleanStructureM .*. withElsePart cleanStructureM

-- TODO does not work yet
removeReturnCondS :: LabeledStrategy (Context Fragment)
removeReturnCondS = label "removeReturnCondS"  
    (
        liftToContext removeIfReturnCond
    .|. liftToContext removeBoolReturnVar
    .|. liftToContext removeIfElseReturnCond
    )

-- SSI-IFRC
-- if(p) return true; else return false :~> return p
removeIfElseReturnCond :: Rule Fragment
removeIfElseReturnCond = makeRule "removeIfElseReturnCond" (\f -> changeFragment f <$> detectIfElseReturnCond' (stats f) )
    where
        detectIfElseReturnCond' stats = do
            (pre, (c, bT, bF))  <- match (matchLastWith
                (matchIfElseWith matchId matchSingleBoolReturn matchSingleBoolReturn) ) stats
            guard (bT /= bF) -- check for opposite boolean values
            return $ makeReturn pre bT c 
                    

{-removeIfElseReturnCond :: [RewriteRule Statement]
removeIfElseReturnCond = makeRewriteRules "removeIfElseReturnCond" 
    [ \c -> IfElse c (Return (Just trueLt)) (Return (Just falseLt)) :~> Return (Just c)
    , \c -> IfElse c (Return (Just falseLt)) (Return (Just trueLt)) :~> Return (Just (nt c)) 
    ]
    -}

-- SSI-IFRT
-- if(p) return true; return false :~> return p
removeIfReturnCond :: Rule Fragment
removeIfReturnCond = makeRule "removeIfReturnCond" (\f -> changeFragment f <$> removeIfReturnCond' (stats f) )
    where
        removeIfReturnCond' stats = do
            (pre, (c, b2), b1)  <- match (matchLast2  -- TODO cleaning
                (matchIfWith matchId matchSingleBoolReturn)
                matchAsBoolReturn) stats
            guard (b1 /= b2) -- check for opposite boolean values
            return $ makeReturn pre b2 c 

-- SSI: IFASNB
-- if(p) { b = true; } else { b = false; } return b; :~> return p;
removeBoolReturnVar :: Rule Fragment
removeBoolReturnVar = makeRule "removeBoolReturnVar" (\f -> changeFragment f <$> removeBoolReturnVar' (stats f) )
    where
        removeBoolReturnVar' stats = do
            let matchSingleBoolAssign = matchSingleBlockWith matchId matchBoolAssign
            (pre, (c, (v1, b1), (v2, b2)), v)  <- match (matchLast2 
                (matchIfElseWith matchId matchSingleBoolAssign matchSingleBoolAssign) 
                matchAsIdtReturn) stats
            guard (b1 /= b2 && v1 == v2 && v == v1)
            return $ makeReturn pre b1 c 

makeReturn :: [Statement] -> Bool -> Expression -> [Statement]
makeReturn pre b c = pre ++ [Return (Just $ if b then c else nt c)]

------------------------------------------------------------
-- Rules on expressions 

-- Boolean ------------------------------------------------- 

boolExprRefS :: LabeledStrategy (Context Expression)
boolExprRefS = label "boolExprRefS" $
         choice (useRs [equalsTrue, equalsFalse, notEqualsTrue, notEqualsFalse, doubleNeg, idemAnd, idemOr, notTrue, notFalse])
     .|. choice [simplifyComposedS, absorbS, simplifyTrueFalseS]

deMorganAnd :: RewriteRule Expression
deMorganAnd = makeRewriteRule "deMorganAnd" $   
    \p q -> nt (p .&&. q) :~> nt p .||. nt q

deMorganOr :: RewriteRule Expression
deMorganOr = makeRewriteRule "deMorganOr" $ 
    \p q -> nt (p .||. q) :~> nt p .&&. nt q

doubleNeg :: RewriteRule Expression
doubleNeg = makeRewriteRule "doubleNeg" $ 
    \p -> nt (nt p) :~> p

equalsTrue :: RewriteRule Expression
equalsTrue = makeRewriteRule "equalsTrue" $ 
    \p -> p .==. trueLt :~> p

equalsFalse :: RewriteRule Expression
equalsFalse = makeRewriteRule "equalsFalse" $ 
    \p -> p .==. falseLt :~> nt p

notEqualsTrue :: RewriteRule Expression
notEqualsTrue = makeRewriteRule "notEqualsTrue" $ 
    \p -> p .!=. trueLt :~> nt p

notEqualsFalse :: RewriteRule Expression
notEqualsFalse = makeRewriteRule "notEqualsFalse" $ 
    \p -> p .!=. falseLt :~> p

idemAnd :: RewriteRule Expression
idemAnd = makeRewriteRule "idemAnd" $
    \p -> p .&&. p :~> p

idemOr :: RewriteRule Expression
idemOr = makeRewriteRule "idemOr" $
    \p -> p .||. p :~> p

notTrue :: RewriteRule Expression
notTrue = makeRewriteRule "notTrue" $
    nt trueLt :~> falseLt

notFalse :: RewriteRule Expression
notFalse = makeRewriteRule "notFalse" $
    nt falseLt :~> trueLt

simplifyComposedS :: Strategy (Context Expression)
simplifyComposedS = somewhere $ option comm .*. option (withRightOp comm) .*. simplifyComp
    where
        comm          = use (mR commOr .|. mR commAnd)
        simplifyComp  = use $ choice $ rs simplifyComposed

simplifyComposed :: [RewriteRule Expression]
simplifyComposed = makeRewriteRules "simplifyComposed" 
    [ \p q -> p .||. (nt p .&&. q)  :~> p .||. q
    , \p q -> p .&&. (nt p .||. q)  :~> p .&&. q
    , \p q -> nt p .&&. (p .||. q)  :~> nt p .&&. q
    , \p q -> nt p .||. (p .&&. q)  :~> nt p .||. q
    ]

distrAnd :: [RewriteRule Expression]
distrAnd = makeRewriteRules "distrAnd" 
    [ \p q r -> p .&&. (q .||. r)  :~> (p .&&. q) .||. (p .&&. r)
    , \p q r -> (q .||. r) .&&. p  :~> (p .&&. q) .||. (p .&&. r)
    ]

distrOr :: [RewriteRule Expression]
distrOr = makeRewriteRules "distrOr" 
    [ \p q r -> p .||. (q .&&. r)  :~> (p .||. q) .&&. (p .||. r)
    , \p q r -> (q .&&. r) .||. p  :~> (p .||. q) .&&. (p .||. r)
    ]

absorbS :: Strategy (Context Expression)
absorbS = somewhere $ option comm .*. option (withRightOp comm) .*. absorb
    where
        comm          = use (mR commOr .|. mR commAnd)
        absorb        = use (rr absorbAnd .|. rr absorbOr)

absorbAnd :: RewriteRule Expression
absorbAnd = makeRewriteRule "absorbAnd" $
    \p q -> p .&&. (p .||. q)  :~> p

absorbOr :: RewriteRule Expression
absorbOr = makeRewriteRule "absorbOr" $
    \p q -> p .||. (p .&&. q)  :~> p

-- Remove?
reorder :: [RewriteRule Expression]
reorder = makeRewriteRules "reorder" 
    [ \p q r ->  (p .&&. q) .||. r :~> r .||. (p .&&. q)
    , \p q r -> (p .||. q) .&&. r  :~> r .&&. (p .||. q)
    ]

commAnd :: RewriteRule Expression
commAnd = makeRewriteRule "commAnd" $
    \p q -> p .&&. q :~> q .&&. p

commOr :: RewriteRule Expression
commOr = makeRewriteRule "commOr" $
    \p q -> p .||. q :~> q .||. p

assocAnd :: [RewriteRule Expression]
assocAnd = makeRewriteRules "assocAnd" 
    [ \p q r -> p .&&. (q .&&. r) :~> (p .&&. q) .&&. r
    , \p q r -> (p .&&. q) .&&. r :~> p .&&. (q .&&. r)
    ]

assocOr :: [RewriteRule Expression]
assocOr = makeRewriteRules "assocOr" 
    [ \p q r -> p .||. (q .||. r) :~> (p .||. q) .||. r
    , \p q r -> (p .||. q) .||. r :~> p .||. (q .||. r)
    ]

simplifyTrueFalseS :: Strategy (Context Expression)
simplifyTrueFalseS = somewhere $ option comm .*. simpl
    where
        comm     = use (mR commOr .|. mR commAnd)
        simpl    = use $ choice $ rs $ simplifyAnd ++ simplifyOr

simplifyAnd :: [RewriteRule Expression]
simplifyAnd = makeRewriteRules "simplifyAnd"
    [ \p -> p .&&. trueLt  :~> p
    , \p -> p .&&. falseLt :~> falseLt
    , \p -> p .&&. nt p    :~> falseLt
    ]

simplifyOr :: [RewriteRule Expression]
simplifyOr = makeRewriteRules "simplifyOr"
    [ \p -> p .||. trueLt  :~> trueLt
    , \p -> p .||. falseLt :~> p
    , \p -> p .||. nt p    :~> trueLt
    ]
    
buggyEqualsTrue :: Rule Expression
buggyEqualsTrue = buggy . ruleRewrite . makeRewriteRule "buggyEqualsTrue" $ 
    \p -> p .==. trueLt :~> trueLt

extractNot :: [RewriteRule Expression]
extractNot = makeRewriteRules "extractNot"
    [ \a b -> a .!=. b  :~> nt (a .==. b)
    , \a b -> a .>=. b  :~> nt (a .<. b)
    , \a b -> a .<=. b  :~> nt (a .>. b)
    , \a b -> a .>. b   :~> nt (a .<=. b)
    , \a b -> a .<. b   :~> nt (a .>=. b)
    ]

-- Arithmetic ----------------------------------------------

arithExprRefS :: Strategy (Context Expression)
arithExprRefS = introduceCompoundS 
    .|. label "improveEvenCheckS" (use (rr improveEvenCheck))
    .|. label "pushNot" (use . choice . rs $ pushNot)

replaceCompoundOp :: [RewriteRule Expression]
replaceCompoundOp = makeRewriteRules "replaceCompoundOp"  
    [ \l r -> l .+=. r :~>  l .=. (l .+. r)
    , \l r -> l .-=. r :~>  l .=. (l .-. r)
    , \l r -> l .*=. r :~>  l .=. Infixed Multiplication l r
    , \l r -> l ./=. r :~>  l .=. Infixed Division l r
    , \l r -> l .%=. r :~>  l .=. Infixed Remainder l r
    ]

replaceIncrement :: RewriteRule Expression
replaceIncrement = makeRewriteRule "replaceIncrement" $ 
    \l -> (.++.) l :~> l .=. (l .+. i1Lt)

replaceDecrement :: RewriteRule Expression
replaceDecrement = makeRewriteRule "replaceDecrement" $ 
    \l -> (.--.) l :~> l .=. (l .-. i1Lt)

-- Not for composed expr such as p = (p + 2) + a
introduceCompoundS :: LabeledStrategy (Context Expression)
introduceCompoundS = label "introduceCompoundS" $ somewhere 
    $       try (useMR operandsOrder)
        .*. use (rr introduceIncrement |> choice (rs introduceCompoundOp))

introduceCompoundOp :: [RewriteRule Expression]
introduceCompoundOp = makeRewriteRules "introduceCompoundOp" 
    [ \l r -> l .=. (l .+. r) :~> l .+=. r
    , \l r -> l .=. (l .-. r) :~> l .-=. r
    , \l r -> l .=. (Infixed Multiplication l r) :~> l .*=. r
    , \l r -> l .=. (Infixed Division l r)       :~> l ./=. r
    ]

introduceIncrement :: RewriteRule Expression
introduceIncrement = makeRewriteRule "introduceIncrement" $ 
    \l -> l .=. (l .+. i1Lt) :~> (.++.) l

introduceDecrement :: RewriteRule Expression
introduceDecrement = makeRewriteRule "introduceDecrement" $ 
    \l -> l .=. (l .-. i1Lt) :~> (.--.) l

-- s = a + s => s = s + a
operandsOrder :: RewriteRule Expression
operandsOrder = makeRewriteRule "operandsOrder" $
    \l r -> l .=. (r .+. l) :~> l .=. (l .+. r)

splitCombinedE :: [RewriteRule Expression]
splitCombinedE = makeRewriteRules "splitCombinedE"  
    [ \l r -> l .>=. r :~>  (l .==. r) .||. (l .>. r)
    , \l r -> l .<=. r :~>  (l .==. r) .||. (l .<. r)
    ]

-- Changes semantics for neg. numbers
improveEvenCheck :: RewriteRule Expression
improveEvenCheck = makeRewriteRule "improveEvenCheck" $ 
    \l -> (l .%. i2Lt) .!=. i1Lt   :~>   (l .%. i2Lt) .==. i0Lt

pushNot :: [RewriteRule Expression]
pushNot = makeRewriteRules "pushNot" 
    [ \l r -> nt (l .==. r)   :~>   l .!=. r
    , \l r -> nt (l .!=. r)   :~>   l .==. r
    , \l r -> nt (l .>. r)    :~>   l .<=. r
    , \l r -> nt (l .>=. r)   :~>   l .<. r
    , \l r -> nt (l .<=. r)   :~>   l .>. r
    , \l r -> nt (l .<. r)    :~>   l .>=. r
    ]
