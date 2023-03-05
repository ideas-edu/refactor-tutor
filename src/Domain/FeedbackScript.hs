module Domain.FeedbackScript 
(
    extendScript, makeAnnotationScript
)
where

import Ideas.Service.FeedbackScript.Syntax (Script, makeScript, textForIdDecl, Text(..))
import Ideas.Common.Exercise
import Ideas.Common.Library hiding (function, ors)
import Data.List
import Data.Semigroup

-- | Vul het script van de DR (uit file) aan met annotaties en rules
extendScript:: Script -> Exercise a -> Script 
extendScript script ex = script <> makeAnnotationScript ex

-- | Maak een script voor annotaties en rules
makeAnnotationScript:: Exercise a -> Script
makeAnnotationScript ex = makeScript (map makeDecl $ ruleIds ++ annotationIds)
    where       
        ruleIds = map getId $ ruleset ex
        allLabelIds = map (getId . snd) $ strategyLocations $ strategy ex -- TODO anders?
        annotationIds = filter isAnnotation $ nub allLabelIds    
        makeDecl labelId = textForIdDecl labelId . TextString . description $ labelId
        isAnnotation id = 
            "fb." `isPrefixOf` showId id ||
            "desc." `isPrefixOf` showId id

