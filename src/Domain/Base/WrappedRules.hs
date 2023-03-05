------------------------------------------------------------------
-- 
-- Wrapping transformations as rules
-- Created: 30-5-2019
--
------------------------------------------------------------------

module Domain.Base.WrappedRules where

import Domain.Base.AST
import Domain.Base.Transformations 
import Ideas.Common.Library as Ideas
import Data.Maybe
import Utils.Utils

copyPropRule :: Rule BProgram
copyPropRule = makeRule "copyProp" (asMaybe copyProp)

copyProp2Rule :: Rule BStat
copyProp2Rule = makeRule "copyPropIfCondition" copyPropIfCondition

expandIfWithOrRule :: Rule BStat
expandIfWithOrRule = makeRule "expandIfWithOr" (asMaybe expandIfWithOr)

cleanRule :: Rule BStat
cleanRule = makeRule "clean" (asMaybe clean)

reRule :: Rule [BStat]
reRule = makeRule "re" (asMaybe removeEmpties)

removeUnusedVarsRule :: Rule BProgram
removeUnusedVarsRule = makeRule "removeUnusedVars" (asMaybe removeUnusedVars)

