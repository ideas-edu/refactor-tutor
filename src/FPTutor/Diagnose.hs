{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
------------------------------------------------------------------
-- The adjusted deepdiagnose service from the FP-tutor
--
-- Created: 17-10-13 
--

-- Diagnoses:
--   | Buggy          Environment (Rule (Context a))
--   | Missing
--   | IncorrectPart  [a]
--   | NotEquivalent -- "Unknown mistake"
--   | Similar        Bool (State a) -- "Very similar"
--   | Expected       Bool (State a) (Rule (Context a)) -- "Rule " ++ show (show r) ++ ", expected by strategy"
--   | Detour         Bool (State a) Environment (Rule (Context a)) -- "Rule " ++ show (show r) ++ ", not following strategy"
--   | Correct        Bool (State a) --"Unknown step"
         
------------------------------------------------------------------

module FPTutor.Diagnose where

import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
import Ideas.Common.Library hiding (ready, from, to, try)
import Ideas.Common.DerivationTree
import Ideas.Common.Examples (topLevelExamples)
import Ideas.Service.BasicServices hiding (apply)
import Ideas.Service.State
import Ideas.Service.Diagnose hiding (diagnose, newState)
import Ideas.Utils.Prelude (fst3, snd3)
import Utils.Utils
import Domain.Equality
-- from FPTutor
import Ideas.Common.Strategy.Prefix (searchModePrefix)
import Ideas.Common.Strategy.Sequence (firstsTree)
import FPTutor.Utils (try, replayState)
import Data.Typeable 
import qualified Ideas.Common.Rewriting.Difference as Diff

-- Changes:
-- deepdiagnose: buggy/noteq diagnosis
-- expPrefixes: no
-- intermediates: no
-- searchTree: isMajorStep, eqStep different impl.
-- pruneTree: isPredecessor new impl. for Program

deepdiagnose :: State a -> Context a -> Diagnosis a
deepdiagnose s new

   -- Is the submitted term equivalent?
   | not isPrefix = 
        -- Is the rule used discoverable by trying all known buggy rules?
        case discovered True of
           Just (r, as)  -> Buggy as r    -- report the buggy rule
           Nothing       -> NotEquivalent "unknown mistake (not eq)"  -- compareParts state new
              
   -- Is the submitted term (very) similar to the previous one? 
   | similarity ex (stateContext state) new =
        -- If yes, report this
        Similar (finished state) state

  -- old fp
   -- Was the submitted term expected by the strategy?
   {-| not (null $ prefixPaths expected) =
        -- If yes, return new state and rule
        let newState = makeState ex expected new
        in Expected fin newState (idRule "expected rule")-}

    -- new FP
    -- Was the submitted term expected by the strategy?
    | not (null (prefixPaths expPfx)) = 
        -- If yes, return new state and a dummy rule 
        let newState = (makeState ex expPfx new) 
              { stateSession   = stateSession s 
              , stateUser      = stateUser s
              , stateStartTerm = stateStartTerm s }
        in Expected done newState dummyRule

   -- Is the rule used discoverable by trying all known rules?
   | otherwise =
        let newState = restart (emptyStateContext ex new) -- was restartIfNeeded
        in case discovered False of
              Just (r, as) ->  -- If yes, report the found rule as a detour
                 Detour (finished newState) newState as r -- "not following strategy"
              Nothing -> -- If not, we give up
                 -- Correct (finished newState) newState
                 case discovered True of
                  Just (r, as)  -> Buggy as r    -- report the buggy rule
                  Nothing       -> Correct (finished newState) newState
  where
    baseProgram = snd $ head $ topLevelExamples $ examples ex      -- empty program 
    Just sol    = ex `apply` baseProgram           -- a solution for the exercise
    isPrefix    = equivalence ex new (inContext ex sol) 
    
    state = try replayState s
    ex = exercise state
    (expected, fin) = expPrefix state new -- was expPrefixES
  
    -- newFP
    (expPfx, done) = matchingPrefixes ex new
    dummyRule = idRule "expected rule"
    -- end newFP

    discovered searchForBuggy = listToMaybe
        [ (r, env)
        | r <- sortBy (ruleOrdering ex) (ruleset ex)
        , isBuggy r == searchForBuggy
        , (_, env) <- recognizeRule ex r sub1 sub2
        ]
      where 
      diff = if searchForBuggy then difference else differenceEqual
      (sub1, sub2) = fromMaybe (stateContext state, new) $ do
         newTerm <- fromContext new
         (a, b)  <- diff ex (stateTerm state) newTerm
         return (inContext ex a, inContext ex b)

-- new
expPrefix :: State a -> Context a -> (Prefix (Context a), Bool)
expPrefix state newCtx = (mconcat ps, or bs)
  where
    xs   = intermediates state newCtx
    sim  = similarity $ exercise state
    (_, ps, bs) = unzip3 $ filter (sim newCtx . fst3) xs
    

--- new FP

-- | Collect all prefixes that the match target context (== student program)
matchingPrefixes :: Exercise a -> Context a -> (Prefix (Context a), Bool)
matchingPrefixes ex targetCtx = -- (noPrefix, true) geeft wel res.
   biMap mconcat or $ unzip $ rec $ searchTree2 ex
 where
  pre = false --  \ctx -> isPredecessor' ex ctx targetCtx
  sim = similarity ex targetCtx
  rec = concatMap getMatches . branches
  getMatches (ctx, t)
    | sim ctx   = [(root t, endpoint t)]  -- if similar then we have a match
    | pre ctx   = rec t                   -- if predecessor then we recurse and continue search
    | otherwise = []                      -- it not a predecessor then we can stop searching

searchTree2 :: Exercise a -> DerivationTree (Context a) (Prefix (Context a))
searchTree2 ex = mapFirst snd3 $ firstsTree $ searchModePrefix p
 where
   p   = emptyPrefix (strategy ex) ctx
   ctx = inContext ex $ snd $ head $ topLevelExamples $  examples ex -- added tle
{-
isPredecessor' :: Exercise a -> Context a -> Context a -> Bool
isPredecessor' ex x y = norm x == norm y
  where
    -- names = fromMaybe [] $ lookupNames ex
  norm  =  castFromCtx ex  -- normalise names . removeHoles . castFromCtx ex-}

castFromCtx :: Typeable b => Exercise a -> Context a -> b
castFromCtx ex ctx = fromMaybe (error "DeepDiagnose.hs: could not cast!") $ 
    fromContext ctx >>= castFrom ex

--- end new FP


---------
-- Old -- 
---------    
-- return a list of all valid strategy prefixes, + bool if one is finished
--expPrefixes :: State a -> Context a -> ([Prefix (Context a)], Bool)
--expPrefixes state newCtx = (ps, or bs)
--  where
--    xs   = intermediates state newCtx
--    sim  = similarity $ exercise state 
--    (ps, _, bs) = unzip3 $ filter (sim newCtx . snd3) xs -- keep only those similar to new

---------
-- Old -- 
---------
-- return all possible intermediate states+prefix, returned in depth-first order
--intermediates :: State a -> Context a -> [(Prefix (Context a), Context a, Bool)]
--intermediates state newCtx = 
--    dft p $ pruneTree ex newCtx $ searchTree ctx p
--    --dft p $ searchTree ctx p -- no pruning
--  where
--    p   = emptyPrefix (strategy ex) $ inContext ex $ snd $ head $ examples ex -- $ snd... == emptyProgram
--    ex  = exercise state
--    ctx = stateContext state

intermediates :: State a -> Context a -> [(Context a, Prefix (Context a), Bool)]
intermediates state newCtx = 
   dft ctx $ pruneTree ex newCtx $ searchTree p
 where
   p   = emptyPrefix (strategy ex) ctx
   ctx = inContext ex $ snd $ head $ topLevelExamples $ examples ex -- newCtx ???
   ex  = exercise state
   
---------
-- Old -- 
---------
-- HK searchModePrefix=searchModeState in common.strat.parsing
-- tidyProcess, uniquePath in sequential
--searchTree :: a -> Prefix a -> DerivationTree (Prefix a) a
--searchTree ctx prefix = -- flip majorPrefixTree ctx . searchModePrefix isMajorStep eqStep -- !!!!
--    let
--        newPre = searchModePrefix isMajorStep eqStep prefix -- :: Prefix a
--    in majorPrefixTree newPre ctx
--    -- majorPrefixTree prefix ctx -- no search mode

searchTree :: Prefix a -> DerivationTree a (Prefix a)
searchTree = mapFirst snd3 . firstsTree . searchModePrefix -- eqStep, removed ideas 1.4

-- | depth first traversal of a derivation tree
dft :: a -> DerivationTree a b -> [(a, b, Bool)]
dft p t = (p, root t, endpoint t) : concatMap (uncurry dft) (branches t)


-- Old --
-- HK: removes branches cannot be predecessors of the new submission
--pruneTree :: Exercise a -> Context a -> DerivationTree s (Context a) -> DerivationTree s (Context a)
--pruneTree ex newCtx = cutOnTerm (not . p)
--  where
--    p x = let fs = fromMaybe [] $ (makeRef "fixedNames") ? x
--          in liftCtx ex (isPredecessor fs) x newCtx         
----    normCtx    = normalise fs newCtx  -- ideally we would normalise this once, but we don't have the fixedNames at our disposal (needs a change to the Exercise datatype)


pruneTree :: Exercise a -> Context a -> DerivationTree (Context a) s -> DerivationTree (Context a) s
pruneTree ex newCtx = cutOnStep (not . p)
  where
    p x = let fs = fromMaybe [] $ makeRef "fixedNames" ? x
          in liftCtx ex (isPredecessor fs) x newCtx         
--    normCtx    = normalise fs newCtx  -- ideally we would normalise this once, but we don't have the fixedNames at our disposal (needs a change to the Exercise datatype)


-- | lift function to work on 'Context a' using casting
liftCtx :: Typeable b => Exercise a -> (b -> b -> c) -> Context a -> Context a -> c
liftCtx ex f x y = fromMaybe (error "Service.hs: could not cast!") $ do 
   x' <- fromContext x >>= castFrom ex
   y' <- fromContext y >>= castFrom ex
   return $ f x' y'     

--------------------------------------------------------------------------------
-- diff-functions, Removed from Ideas 1.7

-- From Ideas.Service.Diagnose, others from Ideas.Common.Rewriting.Difference 
differenceEqual :: Exercise a -> a -> a -> Maybe (a, a)
differenceEqual ex a b = do
   v <- hasTermView ex
   let simpleEq = equivalence ex `on` inContext ex
   differenceEqualWith v simpleEq a b

differenceEqualWith :: View Term a -> (a -> a -> Bool) -> a -> a -> Maybe (a, a)
differenceEqualWith v eq p q = guard (eq p q) >> diff eq v p q

diff :: (a -> a -> Bool) -> View Term a -> a -> a -> Maybe (a, a)
diff eq v a b = do
   let eqT x y = fromMaybe False $ liftM2 eq (match v x) (match v y)
   (t1, t2) <- diffTerm eqT (build v a) (build v b)
   liftM2 (,) (match v t1) (match v t2)

diffTerm :: (Term -> Term -> Bool) -> Term -> Term -> Maybe (Term, Term)
diffTerm eq = rec
 where
   rec p q =
      case (getFunction p, getFunction q) of
         (Just (s1, ps), Just (s2, qs))
            | s1 /= s2         -> Just (p, q)
            | isAssociative s1 -> (diffA s1 `on` collectSym s1) p q
            | otherwise        -> diffList ps qs
         _  | p == q           -> Nothing
            | otherwise        -> Just (p, q)

   diffList xs ys
      | length xs /= length ys = Nothing
      | otherwise =
           case catMaybes (zipWith rec xs ys) of
              [p] -> Just p
              _   -> Nothing

   diffA s = curry (make . rev . f . rev . f)
    where
      f (p:ps, q:qs) | not (null ps || null qs) &&
                       isNothing (rec p q) &&
                       equal ps qs =
         f (ps, qs)
      f pair = pair

      equal     = eq `on` builder
      rev       = reverse *** reverse
      builder   = foldr1 (binary s)
      make pair =
         case pair of
            ([p], [q]) -> rec p q
            (ps, qs)   -> Just (builder ps, builder qs)

collectSym :: Symbol -> Term -> [Term]
collectSym s a = maybe [a] (uncurry ((++) `on` collectSym s)) (isBinary s a)