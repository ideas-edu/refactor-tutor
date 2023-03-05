------------------------------------------------------------------
-- 
--
-- Created: 11-5-2018
--
------------------------------------------------------------------

module Domain.Refactoring.Util where

import Domain.Syntax
import Ideas.Common.Library as Ideas
import Ideas.Common.Traversal.Navigator
import Data.Maybe
import Data.Time

instance Different Expression where
   different = (LiteralExpr (IntLiteral 0), IdExpr (Identifier "x"))

instance Different Statement where
    different = (Empty, Break)

instance Different ForInit where
    different = (ForInitExpr [], ForInitDecls BoolType [])

instance Different Identifier where
    different = (Identifier {name = "x"}, Identifier {name = "y"})

instance Different DataType where
    different = (BoolType, IntType)

rr :: RewriteRule a -> Rule a
rr = ruleRewrite

rs :: [RewriteRule a] -> [Rule a]
rs = map ruleRewrite

mR :: RewriteRule a -> Rule a 
mR = minor . rr

mRs :: [RewriteRule a] -> [Rule a]
mRs = map mR

useR :: (IsTerm a, IsTerm b) => RewriteRule a -> Rule (Context b)
useR = use . rr

useMR :: (IsTerm a, IsTerm b) => RewriteRule a -> Rule (Context b)
useMR = use . mR

useRs :: (IsTerm a, IsTerm b) => [RewriteRule a] -> [Rule (Context b)]
useRs = map useR

useSomew :: (IsTerm a, IsTerm b) => Rule a -> Strategy (Context b)
useSomew = somewhere . use

useSomews :: (IsTerm a, IsTerm b) => [Rule a] -> [Strategy (Context b)]
useSomews =  map useSomew

exhSomewh :: (IsTerm a, IsTerm b) => [Rule a] -> Strategy (Context b)
exhSomewh = exhaustive . map useSomew

-- | Makes a list of rewrite rules with id rule.1, rule.2 etc.
makeRewriteRules :: (IsId a, RuleBuilder f b) => a -> [f] -> [RewriteRule b]
makeRewriteRules id = zipWith (makeRewriteRule . name) [1..] 
    where
        name n = newId id # newId (show n)

checkC :: (a -> Bool) -> Strategy (Context a)
checkC f = check (maybe False f . currentInContext)

-- | Only applies a strategy when it is applicable
strict :: (Apply s, IsStrategy s) => s a -> Strategy a
strict s = check (applicable s) .*. s

-- | Strategy as rule
strategyAsRule :: (Apply s, IsStrategy s) => String -> s a -> Rule a
strategyAsRule id = makeRule (newId id) . apply



-- Navigation -----------------------------------------------------------------

ruleUp2, ruleUp3 :: Navigator a => Strategy a
ruleUp2 = ruleUp .*. ruleUp
ruleUp3 = ruleUp2 .*. ruleUp

-- for Infixed + Assignment
withRightOp, withElsePart, withIfPart, withIfCondition, withNestedIfElsePart,
    withForInit, withForCondition, withForUpdate :: (Navigator a, IsStrategy s) => s a -> Strategy a
withRightOp s = ruleDown .*. ruleRight .*. ruleRight .*. s .*. ruleUp

withElsePart a          = ruleDownLast .*. a .*. ruleUp
withIfPart   a          = ruleDown .*. ruleRight .*. a .*. ruleUp
withIfCondition a       = ruleDown .*. a .*. ruleUp
withNestedIfElsePart s  = ruleDown .*. withElsePart s .*. ruleUp

withForInit a           = ruleDown .*. a .*. ruleUp
withForCondition a      = ruleDown .*. ruleRight .*. ruleRight .*. a .*. ruleUp
withForUpdate a         = ruleDown .*. ruleRight .*. ruleRight .*. a .*. ruleUp


-- Strategy for rewriting ------------------------------------------------------

tryStrat :: (Apply a, IsTerm b) => a (Context b) -> b -> b
tryStrat a x = 
    let res = applyInNewContext a x
    in  maybe x (fromJust . fromContext) res

applyInNewContext :: (Apply a, IsTerm t) => a (Context t) -> t -> Maybe (Context t)
applyInNewContext a = apply a . newTermContext

-- applyInNewContextTimed :: (Apply a, IsTerm t, Show t) => a (Context t) -> t -> IO (Maybe (Context t))
applyInNewContextTimed a t = do
    t1 <- getCurrentTime
    let res = applyInNewContext a t
    print $ (fromJust . fromContext . fromJust) res
    getCurrentTime >>= printElapsedTime t1
    return res
    where
        printElapsedTime :: UTCTime -> UTCTime -> IO ()
        printElapsedTime t1 t2 = putStrLn $ "[Elapsed time " ++ show (diffUTCTime t2 t1) ++ "]"

newTermContext :: (IsTerm t) => t -> Context t
newTermContext = newContext . termNavigator

tryApply :: (Apply a) => a b -> b -> b
tryApply a b = fromMaybe b (apply a b) 