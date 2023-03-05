module Domain.Refactoring.Services 
(
  -- Adapted services
  diagnoseR, diagnoseTextR, onefirsttextR, allfirststextR
)
where

import Domain.Javac
import Utils.Either

import Ideas.Common.Library hiding (Environment)
import Ideas.Service.State
import Ideas.Service.Diagnose
import Ideas.Service.BasicServices (allfirsts, recognizeRule)
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.FeedbackScript.Run
import Data.Either
import Data.Maybe


onefirsttextR :: Script -> State a -> Maybe String -> (Text, Maybe (State a))
onefirsttextR script old event =
    case allfirststextR script old event of
      []       -> (noHint, Nothing)
      hint:_   -> second Just hint

noHint :: Text
noHint = makeText "There are no more hints!"

allfirststextR :: Script -> State a -> Maybe String -> [(Text, State a)]
allfirststextR script old event = map f nexts
  where
    -- ??-spt    = script -- flip addNameScript script
    evt    = newId $ fromMaybe "step" event
    nexts  = either (const []) id $ Ideas.Service.BasicServices.allfirsts old --allfirsts :: State a -> Either String [(StepInfo a, State a)]
    -- f step = ( feedbackHint evt (createEnv old (fst3 (fst3 step)) (snd3 step)) spt -- spt => (spt $ thd3 step)
    --          , snd3 step )
    f ((rule, loc, env), s) = (feedbackHint (getId rule) (createEnv old rule s) script, s) -- (newEnvironment s Nothing)newId "hint"hint of step

-- copied from FPTutor
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

diagnoseR :: State a -> String -> IO (Diagnosis a)
diagnoseR state new = 
    -- if the code cannout be parsed, check if the Java compiler has a better msg
    case tryParse of
        Left msg -> fromMaybe (SyntaxError msg) <$> runJavaCompiler
        Right _  -> rest
    where
        runJavaCompiler :: IO (Maybe (Diagnosis a))
        runJavaCompiler = do
            res <- runJavac new (fromMaybe "noUser" $ stateUser state)
            return $ if hasJavacOutput res 
              then Just (SyntaxError $ processJavacOutput res)
              else Nothing

        rest       
            -- | isLeft tryParse = SyntaxError (forceLeft tryParse)

            -- Buggy?
            | isJust (discovered True Nothing) = 
                  let Just (r, as) = discovered True Nothing
                  in return $ Buggy as r 

            -- | not (isSatisfied (constraints ex !! 0) parsedNew) =
            --    let violatedC = fromJust . flip isViolated parsedNew . (!!0) . constraints 
            --    in NotEquivalent ("Constraint violated: " ++ violatedC ex)

            -- Test cases?
            | (not . and) (checkConstraints (constraints ex)) = do
                let violatedC = head $ mapMaybe (`isViolated` parsedNew) $ constraints ex
                if take 4 violatedC == "Test" -- TODO cases like params/infinte loops
                    then return (NotEquivalent violatedC) -- an actual test case failed
                    else fromMaybe (NotEquivalent violatedC) <$> runJavaCompiler -- try javac for possible explanation
                            
         --    | not (equivalence ex (stateContext state) parsedNew) = NotEquivalent "Not all test cases passed"

            -- Similar?
            -- moved for empty exercises
            | similarity ex (stateContext state) parsedNew = return $ Similar (finished state) state

            -- Recognised step?
            | isJust (expected parsedNew)  =
                -- return new state and rule
                let ((r, _, _), ns) = fromJust (expected parsedNew)
                in return $ Expected (finished ns) ns r

            -- | otherwise = let newState = restart (emptyStateContext ex parsedNew)
            --              in Correct (finished newState) newState
            
            | otherwise =
                  -- detour takes too long?
                 --case discovered False Nothing of
                 --  Just (r, as) ->  -- If yes, report the found rule as a detour
                  --    Detour (finished restarted) restarted as r
                 --  Nothing -> -- If not, we give up
                      return $ Correct (finished restarted) restarted

        tryParse = parser ex new -- :: Either String ClassMember

        ex = exercise state
       
        parsedNew = inContext ex $ forceRight tryParse

        restarted = restart state {stateContext = parsedNew}

        expected new = do
          let xs = either (const []) id $ allfirsts state
              p (_, ns) = similarity ex new (stateContext ns) -- use rule recognizer?
          listToMaybe (filter p xs)
       
        checkConstraints = map (`isSatisfied` parsedNew)

        discovered searchForBuggy searchForRule = listToMaybe
          [ (r, env)
          | r <- (ruleset ex) -- sortBy (ruleOrdering ex) 
          , isBuggy r == searchForBuggy
          , maybe True (`elem` getId r:ruleSiblings r) searchForRule
          , (_, env) <- recognizeRule ex r sub1 sub2
          ]
          where
            new = parsedNew
            (sub1, sub2) = fromMaybe (stateContext state, new) $ do
               newTerm <- fromContext new
               (a, b)  <- difference ex (stateTerm state) newTerm
               return (inContext ex a, inContext ex b)   

getRule :: Diagnosis a -> Maybe (Rule (Context a))
getRule d = case d of 
    Buggy _ r        -> Just r
    Expected _  _ r  -> Just r
    Detour _ _ _ r   -> Just r
    WrongRule _ _ mr -> mr
    _                -> Nothing

diagnoseTextR :: Script -> State a -> String -> IO (Diagnosis a, Text)
diagnoseTextR script old ctx = do
   diagnosis <- diagnoseR old ctx
   return (diagnosis, feedbackDiagnosis diagnosis (newEnvironment old Nothing) script)

diagnoseTextRold :: Script -> State a -> String -> IO (Bool, Text, State a, Bool, String)
diagnoseTextRold script old ctx = do
   diagnosis <- diagnoseR old ctx
   return $ case diagnosis of
      Buggy _ _       -> (False, output diagnosis, old, False, "buggy")
      NotEquivalent _ -> (False, output diagnosis, old, False, "notequivalent")
      Expected r s _  -> (True , output diagnosis, s, r, "expected") 
      Similar r s     -> (True , output diagnosis, s, r, "similar")
      Detour r s _ _  -> (True , output diagnosis, s, r, "detour")
      Correct r s     -> (False, succMsg, s, r, "correct")
      Unknown r s     -> (False, gaveUpMsg, s, r, "unknown")
      WrongRule{}     -> (False, output diagnosis, old, False, "wrongrule")
      SyntaxError e   -> (False, TextString "syntax error", old, False, "syntaxerror" )
 where
   
--   state     = fromMaybe old (diagnoseState diagnosis)
   output  diagnosis  = feedbackDiagnosis diagnosis env script
   ex  = exercise old
   env = newEnvironment old Nothing
    {-(newEnvironment old Nothing)
            { diffPair = do
                 oldC     <- fromContext (stateContext old)
                 a        <- fromContext ctx
                 (d1, d2) <- difference ex oldC a
                 return (prettyPrinter ex d1, prettyPrinter ex d2)
            }
            -}
   succMsg   = TextString "Your solution passed all tests! However, it is \
                          \different from the solutions we have constructed."
   gaveUpMsg = TextString "You have drifted from the strategy in such a way \
                          \that we can not help you any more."