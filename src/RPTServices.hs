{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances  #-}
module RPTServices (rptServiceList) where

import Prelude hiding (sequence)
import Ideas.Common.Library hiding (ready, try)
import Ideas.Service.Types
import qualified Ideas.Service.ServiceList as S
import qualified Ideas.Service.Diagnose as Diagnose 
import Ideas.Service.BasicServices (tStepInfo)

import FPTutor.Services (allhints)
import FPTutor.Utils (try, replayState)

import Domain.Refactoring.Services (diagnoseR, diagnoseTextR, onefirsttextR, allfirststextR)
import Utils.Utils


-- | Services for refactoring
rptServiceList :: [Service]
rptServiceList = [ onefirsttextRS, allfirststextRS, allhintsS
    , diagnoseRS, diagnoseTextRS
    , expandHint, hintsRemaining ]

diagnoseRS :: Service
diagnoseRS  = makeService "diagnoseR" 
   "Diagnose for refactoring" $
   diagnoseR ::: tState .-> tString .-> tIO Diagnose.tDiagnosis

diagnoseTextRS :: Service
diagnoseTextRS  = makeService "diagnoseTextR" 
   "Text diagnose for refactoring" $
   diagnoseTextR ::: tScript .-> tState .-> tString .-> tIO (tPair Diagnose.tDiagnosis tText) -- (tTuple5 tBool tText tState tBool tString)
 where
   --diagnoseTextR' script state = diagnoseTextR script (try replayState state)

onefirsttextRS :: Service
onefirsttextRS = makeService "onefirsttextR" 
   "Similar to the onefirst service, except that the result is now returned as \
   \a formatted text message. The optional string is for announcing the event \
   \leading to this service call (which can influence the returned result). \
   \The boolean in the result specifies whether a suggestion was available or \
   \not." $ 
   onefirsttextR ::: tScript .-> tState .-> tMaybe tString .-> tPair tText (tMaybe tState) 

allfirststextRS :: Service
allfirststextRS = makeService "allfirststextR" 
   "Similar to the allfirsts service, except that the results are now returned as \
   \a formatted text message. The optional string is for announcing the event \
   \leading to this service call (which can influence the returned result). \
   \The boolean in the result specifies whether a suggestion was available or \
   \not." $ 
   allfirststextR ::: tScript .-> tState .-> tMaybe tString .-> tList (tPair tText tState)

-- Store UI actions for logging (state, node clicked (prefix numbering), expand/alt, node text clicked, node text revealed)
expandHint :: Service
expandHint = makeService "expandHint" "Expand on a hint, or ask for an alternative" $ 
    expandHint' ::: tState .-> tInt .-> tString .-> tString .-> tString .-> tUnit
    where
      expandHint' _ _ _ _ _ = ()

hintsRemaining :: Service
hintsRemaining = makeService "hintsRemaining" "The number of improvements left." $ 
   hintsRemaining' ::: tScript .-> tState .-> tMaybe tString .-> tInt
   where
      hintsRemaining' sc st ms = length $ allfirststextR sc st ms

stepsremainingS :: Service
stepsremainingS = getService "stepsremaining"

allhintsS :: Service
allhintsS = makeService "allhints" 
  "Returns a tree of feedback messages with increasing specificity" $ 
  allhints ::: tScript .-> tState .-> tTree (tPair tText tText)

-- Help functions
getService :: String -> Service
getService name = 
    case filter ((name ==) . unqualified . getId) S.serviceList of 
      [s] -> changeId (newId . unqualified) s
      _   -> error $ "No such service: " ++ name