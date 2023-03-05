module FPTutor.Utils 
(
    try, replayState, safeHead, restoreHoles
    -- eqStep, -- removed ideas1.4
)
where

import Data.Maybe
import Ideas.Common.Library hiding (try, bindings, vars, check)
import Ideas.Common.Examples (topLevelExamples)
-- import Ideas.Common.Strategy.Abstract (LabelInfo)
import Ideas.Service.State
--import Ideas.Common.Strategy.Parsing (choices)
import Control.Monad (forM, liftM)

-- new

restoreHoles :: State a -> Maybe (State a)
restoreHoles state = do
    newState <- replayState state
    return $ replaceTerm oldTerm newState
  where
    oldTerm = stateTerm state

replaceTerm :: a -> State a -> State a
replaceTerm x state =  state { stateContext = newCtx }
  where
    newCtx = applyTop (const x) $ stateContext state
      
    
-- Changed for IPT

--eqStep :: Step a -> Step a -> Bool
--eqStep x y =  
--    case (x, y) of
--        (Enter l1,      Enter l2)      -> l1 == l2
--        (Exit l1,       Exit l2)       -> l1 == l2
--       (RuleStep _ r1, RuleStep _ r2) -> r1 == r2 && isRefineRule r1 && isRefineRule r2
--        (_,             _)             -> False

---------
-- Old, wordt niet meer gebruikt in dd -- 
---------
-- bij isMajor gaat sharedPrefix mis
--isMajorStep :: Step LabelInfo a -> Bool
--isMajorStep (RuleStep _ r) = True --isMajor r
--isMajorStep _              = False


-- Unchanged from FPTutor
--
-- | Create a derivation tree based on major rules with a "prefix" as annotation
--majorPrefixTree :: Prefix a -> a -> DerivationTree (Prefix a) a
--majorPrefixTree pfx = 
--  mergeMaybeSteps . mapFirst f . flip prefixTree pfx
-- where
--   f p = do
--     step <- lastStepInPrefix p 
--     case step of
--       RuleStep _ r -> if isMajor r then Just p else Nothing
--       _            -> Nothing
       
try :: (a -> Maybe a) -> a -> a
try f a = fromMaybe a $ f a

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- From FP.Service.Utils

---------
-- Old -- 
---------
-- Replay the state from a list of prefixes
--replayState :: State a -> Maybe (State a)
--replayState state = 
--    pathsToState (map choices $ statePrefixes state) (exercise state)

-- Replay the state from a list of prefixes
replayState :: State a -> Maybe (State a)
replayState state = pathsToState ps ex
  where
    ex = exercise state
    ps = prefixPaths (statePrefix state)
    
--pathsToState :: [Path] -> Exercise a -> Maybe (State a)
--pathsToState ps ex = do
--    ctx  <- startTerm ex
--    ps'  <- mapM (replayPath (strategy ex) ctx) ps
--    ctx' <- safeHead ps' >>= retrace ctx
--    return $ makeState ex ps' ctx'
pathsToState :: [Path] -> Exercise a -> Maybe (State a)
pathsToState paths ex = do
    ctx <- startTerm ex
    prs <- forM paths $ \path -> 
              replayStrategy path (strategy ex) ctx
    res <- liftM fst (safeHead prs)
    return $ makeState ex (mconcat (map snd prs)) res
    
startTerm :: Exercise a -> Maybe (Context a)
startTerm ex = fmap (inContext ex . snd) $ safeHead $ topLevelExamples $ examples ex

---------
-- Old, unused -- 
---------
--replayPath :: LabeledStrategy a -> a -> Path -> Maybe (Prefix a)
--replayPath s x p = makePrefix p s x >>= replayPrefix s x 
--
--retrace :: a -> Prefix a -> Maybe a
--retrace x pfx = applyList (prefixToSteps pfx) x
--
---- Replay a prefix from a start term by applying all rules from a prefix
--replayPrefix :: LabeledStrategy a -> a -> Prefix a -> Maybe (Prefix a)
--replayPrefix s x pfx =
--    retrace x pfx >>= makePrefix (choices pfx) s

