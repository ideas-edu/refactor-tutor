{-# LANGUAGE FlexibleContexts #-}
module Domain.Base.Normalisation where

import Domain.Syntax
import Domain.Base.AST as B
import Domain.Base.Conversion
import Domain.Base.ExprTransformations hiding ((.*.))
import Domain.Base.Transformations
import Domain.Base.WrappedRules
import Domain.Base.Rules
import Domain.Refactoring.Util
-- temp
import Domain.Printer
import Domain.Parsers.JavaParser

import Ideas.Common.Environment
import Ideas.Common.Library

import Data.Generics.Uniplate.DataOnly (transformBi)
import Data.Maybe
import Utils.Utils


exprTrans :: BProgram -> BProgram
exprTrans = 
      transformBi viewAsCNF 
    . transformBi viewAsMul 
    . transformBi viewAsSum    
    . transformBi viewAsUnEq 
    . transformBi viewAsEq
    
normExpr = loopTransformations exprTrans  --  preparedProgram

normalisePartial :: (ToBase a) => a -> (BProgram, Environment)
normalisePartial = 
    let
        -- Perform only once
        initialise = toB
        finalise = renameVars

    in finalise . loopTransformations transformations . initialise


normaliseCM :: ClassMember -> (BClassMember, Environment)
normaliseCM = 
    let
        -- Perform only once
        initialise = conv
        finalise x = (x, undefined)
        transformations = undefined

    in finalise . loopTransformations transformations . initialise

transformations :: BProgram -> BProgram
transformations = 
    -- program transformations
    
    -- statement transformations

    --    transformBi createBlock
    -- .   transformBi separateVarDecls
       transformBi loopAt0 -- problems with unfinshed loops that do not start @0
--            .   transformBi while2for 
    .  transformBi clean . transformBi (fromMaybeId copyPropIfCondition) . transformBi removeEmpties . transformBi goe
    -- .  tryStrat tryRemoveUnneededIfConditionsS
    . tryStrat (somewhere $ use extractFromIf )
    . transformBi removeUnusedVars
    . transformBi simplifyIf
    . transformBi (fromMaybeId removeContinue)
   -- .  transformBi ifElseFinally
    -- expression transformations
    .   (fst. renameVars)
    . exprTrans
    .   (renameRedefinedVars . copyProp)

transformationsS :: BStat -> BStat
transformationsS = 
      transformBi loopAt0
    . transformBi clean . transformBi (fromMaybeId copyPropIfCondition) . transformBi removeEmpties . transformBi goe
    . transformBi removeUnusedVars
    . transformBi simplifyIf
    . (transformBi viewAsCNF . transformBi viewAsMul . transformBi viewAsSum . transformBi viewAsUnEq . transformBi viewAsEq) 

ts :: [BStat] -> [BStat]
ts = 
      transformBi clean . transformBi removeEmpties . transformBi goe
    . transformBi removeUnusedVars
    . (transformBi viewAsCNF . transformBi viewAsMul . transformBi viewAsSum . transformBi viewAsUnEq . transformBi viewAsEq)

-- TODO improve
transRule :: Rule [BStat]
transRule = makeRule "ts" (asMaybe ts) 

-- Max if with 3 conditions
tryRemoveUnneededIfConditionsS :: Strategy (Context BFragment)
tryRemoveUnneededIfConditionsS = 
    somewhere (repeat1 (use expandIfWithOrRule) 
      .*. (   -- check cond1
            ifCPStratTopLevel
              .|.
                -- check cond2 in elseif
                withElsePart (
                  ifCPStratNested'
                   .|.
                      -- check cond3 in elseif-elseif
                      withNestedIfElsePart ifCPStratNested'
                )
          )
        .*. try (useR combineIfConditionsRule) -- combine remaining conditions
    )
    where 
        ifCPStrat :: Strategy (Context BFragment)    
        ifCPStrat = use copyProp2Rule 
            .*. withIfPart (use $ repeatS transRule)      -- try to clean up

        ifCPStratTopLevel :: Strategy (Context BFragment)    
        ifCPStratTopLevel = ifCPStrat .*. useR emptyIfTrueRule
       
       -- boven if
        ifCPStratNested' :: Strategy (Context BFragment)    
        ifCPStratNested' = ruleDown .*. ifCPStrat .*. ((useR emptyIfTrueRule .*. ruleUp) |> (ruleUp .*. use reRule))

truicExercise :: Exercise BProgram
truicExercise = emptyExercise
    {   
        parser         = fmap toB . parseFragment, 
        prettyPrinter  = show . pretty,
        -- ready          = true,
        suitable       = true,
        -- strategies and rules
        strategy       = label "toaz" (somewhere $ useC tryRemoveUnneededIfConditionsS),
        navigation     = termNavigator
    }

-- for removeUnneededIfConditionsS, diff context
ifCPStrat :: Strategy (Context BStat)   
ifCPStrat = use copyProp2Rule .*. withIfPart (use $ repeatS transRule)

ifCPStratTopLevel :: Strategy (Context BStat)   
ifCPStratTopLevel = ifCPStrat .*. useR emptyIfTrueRule

ifCPStratNested :: Strategy (Context BStat)    
ifCPStratNested = ifCPStrat  .*. checkC isEmptyIf



