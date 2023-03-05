{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-- From AdaptedServices/FPTutor, ExtraServices
-- 
module FPTutor.Services 
( -- * Adapted services
    allfirsts, onefirst, feedbacktextdeep, onefirsttext, allfirststext,
    labels, allhints
    --, allhints2, improve
    , showLocation
)
where

import Data.Function (on)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Tree
import Control.Monad
import Control.Applicative ((<|>))
import Ideas.Common.Library hiding (try, Environment, ready, from, to, output)
import Ideas.Utils.Prelude (fst3, snd3, thd3)
import Ideas.Service.BasicServices (StepInfo)
import Ideas.Service.Diagnose
import Ideas.Service.State
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.FeedbackScript.Run
import Ideas.Common.Traversal.Navigator
import FPTutor.Utils (safeHead)
import FPTutor.Diagnose
import Domain.Syntax
import Domain.FeedbackScript
import Domain.Printer


type Name = String
type NameMap = M.Map Name Name

-- Adapted services ------------------------------------------------------------

-- Changed for RPT, don't need try restoreHoles
allfirsts :: State a -> Either String [(StepInfo a, State a, NameMap)]
allfirsts state
   | withoutPrefix state = Left "Prefix is required"
   | otherwise = Right $ 
        mergeDuplicates $ map make $ firsts $ state -- try restoreHoles state                   
  where
    ex = exercise state
   
    make ((r, ctx, env), st) =
      let prfx = statePrefix st
          newState = makeState ex prfx $ cleanEnv ctx  -- we clean the env because we don't want to send lots of internal info to the webapp
          nameMap  = M.empty -- calcNameMap state newState        -- after hole mapping remove state and dont merge
      in ((r, location ctx, env), newState, nameMap) -- applyNameMap nameMap newState

    mergeDuplicates = mapMaybe mergeSteps . groupWith eq
    eq = similarity ex `on` (stateContext . snd3)

                    
mergeSteps :: [(StepInfo a, State a, b)] -> Maybe (StepInfo a, State a, b)
mergeSteps xs = do
    (step, state, nm) <- safeHead xs 
    return (step, state { statePrefix = newPrefix }, nm)
  where
    newPrefix = mconcat [ statePrefix st | (_, st, _) <- xs ]

cleanEnv :: Context a -> Context a
cleanEnv = setEnvironment (makeEnvironment [])

groupWith :: (a -> a -> Bool) -> [a] -> [[a]]
groupWith _ []     = []
groupWith f (x:xs) = 
   let (ys, zs) = partition (f x) xs
   in  (x:ys) : groupWith f zs
                     
onefirst :: State a -> Either String (StepInfo a, State a, NameMap)
onefirst state =
  case allfirsts state of
    Right []     -> Left "No step possible"
    Right (hd:_) -> Right hd
    Left msg     -> Left msg

feedbacktextdeep :: Script -> State a -> Context a -> (Bool, Text, State a, Bool, String)
feedbacktextdeep script old ctx =
   case diagnosis of
      Buggy _ _       -> (False, output, old, False, "buggy")
      NotEquivalent _ -> (False, output, old, False, "notequivalent")
      Expected r s _  -> (True , output, s, r, "expected") 
      Similar r s     -> (True , output, s, r, "similar")
      Detour r s _ _  -> (True , output, s, r, "detour")
      Correct r s     -> (False, succMsg, s, r, "correct")
      Unknown r s     -> (False, gaveUpMsg, s, r, "unknown")
      WrongRule{}     -> (False, output, old, False, "wrongrule")
      SyntaxError e   -> (False, TextString "syntax error", old, False, "syntaxerror" )
 where
   diagnosis = deepdiagnose old ctx
--   state     = fromMaybe old (diagnoseState diagnosis)
   output    = feedbackDiagnosis diagnosis env script -- AG, I think we don't need to ehanche the script here (addNameScript script state)
   ex  = exercise old
   env = (newEnvironment old Nothing)
            { diffPair = do
                 oldC     <- fromContext (stateContext old)
                 a        <- fromContext ctx
                 (d1, d2) <- difference ex oldC a
                 return (prettyPrinter ex d1, prettyPrinter ex d2)
            }
   succMsg   = TextString "Your solution passed all tests! However, it is \
                          \different from the solutions we have constructed."
   gaveUpMsg = TextString "You have drifted from the strategy in such a way \
                          \that we can not help you any more."

onefirsttext :: Script -> State a -> Maybe String -> (Text, Maybe (State a))
onefirsttext script old event =
    case allfirststext script old event of
      []       -> (noHint, Nothing)
      (hint:_) -> second Just hint
      
allfirststext :: Script -> State a -> Maybe String -> [(Text, State a)]
allfirststext script old event = map f nexts
  where
    spt    = script -- flip addNameScript script
    evt    = newId $ fromMaybe "step" event
    nexts  = either (const []) id $ allfirsts old
    f step = ( feedbackHint evt (createEnv old (fst3 (fst3 step)) (snd3 step)) spt -- spt => (spt $ thd3 step)
             , snd3 step )

noHint :: Text
noHint = makeText "There are no more hints!"

createEnv :: State a -> Rule (Context a) -> State a -> Environment a             
createEnv st r next = (newEnvironment st Nothing)
   { expected   = Just r
   , diffPair   = dp
   , after      = f next
   , afterText  = g next 
   }
 where
   f s  = fmap (`build` stateTerm s) (hasTermView (exercise s))
   g s  = return $ prettyPrinter (exercise s) $ stateTerm s
   ex   = exercise st
   dp = do
      oldC     <- fromContext (stateContext st)
      a        <- fromContext (stateContext next)
      (d1, d2) <- difference ex oldC a 
      return (prettyPrinter ex d1, prettyPrinter ex d2)

showLocation :: Context a -> Location -> Maybe String
showLocation a loc = do
                        focus <- navigateTo loc a
                        termInFocus <- currentTerm focus
                        inFocus <- fromTerm termInFocus
                        toS inFocus

toS :: Term -> Maybe String
toS t = 
        fmap showPretty (fromTerm t :: Maybe Program) 
    <|> fmap showPretty (fromTerm t :: Maybe ClassMember)
    <|> fmap showPretty (fromTerm t :: Maybe Fragment)
    <|> fmap showPretty (fromTerm t :: Maybe Statement) 
    <|> fmap showPretty (fromTerm t :: Maybe Expression)  
    <|> Just "?"


labels :: State a -> [[Id]]
labels state = map filterRules (stateLabels state)
  where
    rs          = ruleset $ exercise state
    isRule      = flip elem (map getId rs) . getId
    filterRules = filter (not . isRule)

-- adapted feedbackHints call
allhints :: Script -> State a -> Tree (Text, Text)
allhints script state = 
    maybe (Node (noHint, makeText "undefined") []) filterDups labelTree
  where
    nexts     = either error id $ allfirsts state
    nameMap   = M.unions $ thd3 $ unzip3 nexts
    spt       = extendScript script $ exercise state -- script -- addNameScript nameMap script
    env       = newEnvironment state Nothing
    mkText    = fromMaybe noHint . eval env spt . Left . getId -- EDIT, use spt instead of script
    ruleHints = transpose 
              $ map (\s -> map (\t -> (t, makeText s)) --s is hint or step
                $ feedbackHints' (newId s) [((r, loc, e), st)| ((r, loc, e), st, _) <- nexts] state Nothing spt) 
              ["hint", "step"]
    
    labelTree =
      case partitionByHead lss of
        [(l, lss')] -> Just $ rec (l, lss')
        _           -> Nothing
      where
        lss             = concat
                        $ zipWith (\ls u -> map (++u) ls)
                                  (map (map (map (\t -> (mkText t, makeText "label"))) . labels . snd3) nexts)
                                  ruleHints
        rec (l, lss')   = Node l $ map rec $ partitionByHead lss'
        partitionByHead :: Show a => [[a]] -> [(a, [[a]])]
        partitionByHead = map (\((x:xs):xss) -> (x, xs : map tail xss))
                        . groupBy (on (==) (show . head)) . filter (not . null)


filterDups :: Tree (Text, a) -> Tree (Text, a)
filterDups (Node x []) = Node x []
filterDups (Node x ts) = Node x $ nubBy eq $ map filterDups ts
  where
    eq = on (==) (removeEmptyLines . fst . rootLabel)
    removeEmptyLines = filter (/= '\n') . unlines . filter (/= "") . lines . show

-------------------------------------------------------------------------------
-- From Ideas.Service.FeedbackScript.Run, partly adapted for location

feedbackHints' :: Id -> [((Rule (Context a), Location, c), State a)] -> State a -> Maybe (Rule (Context a)) -> Script -> [Text]
feedbackHints' feedbackId nexts state motivationRule script =
   map (\env -> fromMaybe (defaultHint env script) $
     make feedbackId env script) envs
  where
    envs = map (newEnvironmentFor' state motivationRule . Just) nexts

newEnvironmentFor' :: State a -> Maybe (Rule (Context a)) -> Maybe ((Rule (Context a), Location, c), State a) -> Environment a
newEnvironmentFor' st motivationRule next = Env
  { oldReady   = finished st
  , expected   = fmap (\((x,_,_),_) -> x) next
  , motivation = motivationRule
  , recognized = Nothing
  , diffPair   = dp
  , before     = f st 
  , after      = fmap snd next >>= f
  , afterText  = join $ fmap (\((_,x,_),s) -> showLocation (stateContext s) x) next --fmap snd next >>= g
  }
 where
  f s  = fmap (`build` stateTerm s) (hasTermView (exercise s))
  g s  = return $ prettyPrinter (exercise s) (stateTerm s)
  ex = exercise st
  -- does not work
  dp = do
         oldC     <- fromContext (stateContext st)
         newS     <- fmap (stateContext . snd) next
         newC     <- fromContext newS
         (d1, d2) <- difference ex oldC newC
         return (prettyPrinter ex d1, prettyPrinter ex d2)

make :: Id -> Environment a -> Script -> Maybe Text
make feedbackId env script = toText env script (TextRef feedbackId)

toText :: Environment a -> Script -> Text -> Maybe Text
toText env script = eval env script . Right

defaultHint :: Environment a -> Script -> Text
defaultHint env script = makeText $
   case expected env of
      Just r  -> ruleToString env script r
      Nothing -> "Sorry, no hint available."



-- Old------------------------------------------------------------

--
---- changed: namemapping removed
--allfirsts :: State a -> Either String [(StepInfo a, State a, NameMap)]
--allfirsts state
--   | null ps   = Left "Prefix is required"
--   | otherwise =
--        let trees     = map tree ps
--            tree p    = cutOnStep (justMajor . lastStepInPrefix)
--                                  (prefixTree (stateContext state) p)
--            order     = ruleOrdering ex `on` (fst3 . fst3)
--            justMajor = maybe False isMajor
--        in Right $ mergeDuplicates 
--                  $ sortBy order 
--                 $ mapMaybe make $ concatMap derivations trees                     
--  where
--    ps = statePrefixes $ try replayState state
--    ex = exercise state
--
--    make d = do
--      prefixEnd   <- lastStep d
--      --let ctx      = lastTerm d
--      --    newState = makeState ex [prefixEnd] $ cleanEnv ctx  -- we clean the env because we don't want to send lots of internal info to the webapp
--      --    nameMap  = calcNameMap state newState               -- after hole mapping remove state and dont merge
--      case lastStepInPrefix prefixEnd of
--        Just (RuleStep env r) | isMajor r -> return
--          ( ( r
--            , location  $ lastTerm d -- ctx
--            , env )
--          --, applyNameMap nameMap newState
--          --, nameMap
--          , makeState (exercise state) [prefixEnd] (lastTerm d)
--          , M.empty
--          )
--        _ -> Nothing
--
--    mergeDuplicates = mapMaybe mergeSteps . groupWith eq
--
--    groupWith _ []     =  []
--    groupWith f (x:xs) =  let (ys, zs) = partition (f x) xs
--                          in  (x:ys) : groupWith f zs
--
--    eq = similarity ex `on` (stateContext . snd3)
--
--mergeSteps :: [(StepInfo a, State a, b)] -> Maybe (StepInfo a, State a, b)
--mergeSteps xs = do
--    (step, state, nm) <- safeHead xs 
--    return (step, state { statePrefixes = f xs }, nm)
--  where
--    f = concatMap (statePrefixes . snd3)
--
--onefirst :: State a -> Either String (StepInfo a, State a, NameMap)
--onefirst state =
--  case allfirsts state of
--    Right []     -> Left "No step possible"
--    Right (hd:_) -> Right hd
--    Left msg     -> Left msg
--
---- changed, removed quickcheck- part
---- returns (?, text, state, ready, diagnosis)
--feedbacktextdeep :: Script -> State a -> Context a -> (Bool, Text, State a, Bool, Text)
--feedbacktextdeep script old ctx =
--   case diagnosis of
--      Buggy _ _       -> (False, output, old, False, makeText "buggy")
--      NotEquivalent _ -> (False, output, old, False, makeText "noteq")
--      Expected r s _  -> (True , output, s, r, makeText "expected") 
--      Similar r s     -> (True , output, s, r, makeText "similar")
--      Detour r s _ _  -> (True , output, s, r, makeText "detour")
--      Correct r s     -> (False , output, s, r, makeText "correct") -- was quickcheck
--      
--      --Correct r s     -> (True , output, s, r) -- ?
-- where
--   diagnosis = deepdiagnose old ctx
----   state     = fromMaybe old (diagnoseState diagnosis)
--   output    = feedbackDiagnosis diagnosis env script -- AG, I think we don't need to ehanche the script here (addNameScript script state)
--   ex  = exercise old
--   env = (newEnvironment old Nothing)
--            { diffPair = do
--                 oldC     <- fromContext (stateContext old)
--                 a        <- fromContext ctx
--                 (d1, d2) <- difference ex oldC a
--                 return (prettyPrinter ex d1, prettyPrinter ex d2)
--            }
--            
--noHint :: Text
--noHint = makeText "Sorry, no hint available"
--  
--onefirsttext :: Script -> State a -> Maybe String -> (Text, Maybe (State a))
--onefirsttext script old event =
--    case allfirststext script old event of
--      []       -> (noHint, Nothing)
--      (hint:_) -> second Just hint
--
---- changed    
--allfirststext :: Script -> State a -> Maybe String -> [(Text, State a)]
--allfirststext script old event = map f nexts
--  where
--    --script = makeScript $ map (\b@(a, _, _) -> Simple TextForId [getId $ fst3 a] (makeText $"")) nexts -- [Simple TextForId [getId $ fst3 $ fst3 $ head nexts] (makeText "Hoi")]
--    --spt    = flip addNameScript script
--    evt    = newId $ fromMaybe "step" event
--    nexts  = either (const []) id $ allfirsts old
--    env = newEnvironment old Nothing-- (createEnv old (fst3 (fst3 step)) (snd3 step))
--    
--    f step = (feedbackHint evt env script-- (spt $ thd3 step)
--                -- makeText (description $ fst3 $ fst3 step)
--             , snd3 step ) -- feedbackHint :: Id -> Environment a -> Script -> Text
--
--labels :: State a -> [[LabelInfo]]
--labels state =
--    map (filterRules . activeLabels) $ statePrefixes state -- 
--    where
--    rs          = ruleset $ exercise state
--    isRule      = flip elem (map getId rs) . getId
--    filterRules = filter (not . isRule)
--
---- | Generates hints for a given state
--allhints :: Script -> State a -> Tree (Text , Text)-- [[(Text, Text)]]
--allhints defaultScript state = 
--    maybe (Node (noHint, makeText "undefined") []) filterDups labelTree-- texts ++  ruleHints-- [map snd $ concat ruleHints] -- ++
--    where
--    script = extendScript defaultScript $ exercise state
--    allLabels = concatMap (labels . snd3) nexts -- per mogelijke volgende stap een lijst van de labels
--    texts = map (map toOut) allLabels
--    
--    mkText  = fromMaybe noHint . eval env script . Left . getId
--    
--    toOut li = (makeText (showId (getId li)), mkText li)-- (getDesc . getId) li
--    
--    getDesc:: Id -> Text    
--    getDesc id = feedbackHint id env script   
--    nexts     = either error id $ allfirsts state
--    env       = newEnvironment state Nothing
--    --mkText    = fromMaybe noHint . eval env script . Left . getId 
--    ruleHints = transpose 
--              $ map (\s -> map (\t -> (t, makeText s)) 
--                $ feedbackHints (newId s) [((r, loc, e), st)| ((r, loc, e), st, _) <- nexts] state Nothing script)
--                ["hint", "step"]
--    labelTree =
--      case partitionByHead lss of
--        [(l, lss')] -> Just $ rec (l, lss')
--        _           -> Nothing
--      where
--        lss             = concat
--                        $ zipWith (\ls u -> map (++u) ls)
--                                  (map (map (map (\t -> (mkText t, makeText "label"))) . labels . snd3) nexts)
--                                  ruleHints
--        rec (l, lss')   = Node l $ map rec $ partitionByHead lss'
--        partitionByHead :: Show a => [[a]] -> [(a, [[a]])]
--        partitionByHead = map (\((x:xs):xss) -> (x, xs : map tail xss))
--                        . groupBy (on (==) (show . head)) . filter (not . null)      
--                        
--                                        
--allhints2 :: Script -> State a -> Tree (Text, Text)
--allhints2 script state = 
--    maybe (Node (noHint, makeText "undefined") []) filterDups labelTree
--  where
--    nexts     = either error id $ allfirsts state
--    nameMap   = M.unions $ thd3 $ unzip3 nexts
--    spt       = script -- addNameScript nameMap script
--    env       = newEnvironment state Nothing
--    mkText    = fromMaybe noHint . eval env script . Left . getId 
--    ruleHints = transpose 
--              $ map (\s -> map (\t -> (t, makeText s)) 
--              $ feedbackHints (newId s) [((r, loc, e), st)| ((r, loc, e), st, _) <- nexts] state Nothing spt)
--              ["hint", "step"]
--    
--    labelTree =
--      case partitionByHead lss of
--        [(l, lss')] -> Just $ rec (l, lss')
--        _           -> Nothing
--      where
--        lss             = concat
--                        $ zipWith (\ls u -> map (++u) ls)
--                                  (map (map (map (\t -> (mkText t, makeText "label"))) . labels . snd3) nexts)
--                                  ruleHints
--        rec (l, lss')   = Node l $ map rec $ partitionByHead lss'
--        partitionByHead :: Show a => [[a]] -> [(a, [[a]])]
--        partitionByHead = map (\((x:xs):xss) -> (x, xs : map tail xss))
--                        . groupBy (on (==) (show . head)) . filter (not . null)      
--    
--filterDups :: Tree (Text, a) -> Tree (Text, a)
--filterDups (Node x []) = Node x []
--filterDups (Node x ts) = Node x $ nubBy eq $ map filterDups ts
--  where
--    eq = on (==) (removeEmptyLines . fst . rootLabel)
--    removeEmptyLines = filter (/= '\n') . unlines . filter (/= "") . lines . show  
-- 
--improve:: State Program -> Exercise Program
--improve s = (exercise s) { strategy = liftToContext $ label "" allBlocks }
--
