------------------------------------------------------------------
-- 
--
-- Created: 25-4-2018
--
------------------------------------------------------------------

module Domain.Refactoring.Strategy where

import Ideas.Common.Library as Ideas
import Domain.Syntax hiding ((.%.), (./.))
import Domain.Refactoring.Rules.Rules
import Domain.Refactoring.Rules.LoopRules
import Domain.Refactoring.Rules.BuggyRules
-- import Domain.Refactoring.ExpStrategy
import Domain.Refactoring.Util
import Domain.Base.Normalisation
import Domain.Base.Conversion
import Domain.Base.AST (BFragment(..), bstats)
import Data.Maybe
import Control.Monad

-- Strategy that just tries to apply all basic rules/strategies
doAllS :: LabeledStrategy (Context ClassMember)
doAllS = label "all" $ 
  storeArrType .*.  -- store all array types as a minor rule
    repeatS (
      somewhere
      (       
        -- statements
             (useC (checkC cs) .*. use removeEmptyStats)
        -- fragment
        ./. (useC (checkC cf) .*. (use removeEmptyStatsF ./. useC removeAssignToSelfS))  -- niet in somewhere?
        ./.  (useC (checkC ce) .*. ( useC boolExprRefS .|. useC arithExprRefS ) ) 
      )
    ./. somewhere (
        -- conditionals
         useC (checkC isIforIfElse) .*. (   
                 -- removing the if/ifelse
                     (strict (useC removeUselessIfElseS) .|. strict (useC removeUselessIfS)) -- ie-i
                 -- various
                 ./. strict (useC conditionalS) -- notS (useC removeUselessCheckS) .*. -- ie  
                 -- optimise if-conditions  
                 ./. strict (useC removeUselessCheckS)  --ie
                 ./. strict (useC $ label "removeUnneededIfConditionsS" $ strategyAsRule "removeUnneededIfConditionsR" removeUnneededIfConditionsS) -- s-i
                 -- move code from if-bodies
                 ./. use extractFromIfS
                 -- reduce nesting
                 ./. strict (useC simplifyIfWithDuplicationS)
             ) 
        )  
   -- loops
    ./. somewhere (
            strict (useC (checkC (\s -> isWhile s || isFor s)) .*. 
                (use removeLoopByCalc |> use exitLoopEarly |>
                    useC forToForeachRuleCS |> useC forToWhileStrictS |> use replaceBreak) ) 
            )
    -- return
    ./. somewhere (useC (checkC cf) .*. useC removeReturnCondS)
  )

s !|> t = s .|. (notS s .*. t)

ce :: Expression -> Bool
ce _ = true

cf :: Fragment -> Bool
cf _ = true

cm :: ClassMember -> Bool
cm _ = true

cs :: Statement -> Bool
cs _ = true

-- Does not work properly
--tryRemoveUnneededIfConditionsS'' :: Strategy (Context Program)
--tryRemoveUnneededIfConditionsS'' =  liftViewIn viewZ tryRemoveUnneededIfConditionsS

-- Normalises the entire fragment
tryRemoveUnneededIfConditionsS' :: Rule (Context Fragment)
tryRemoveUnneededIfConditionsS' =  makeRule "tryRemoveUnneededIfConditionsS" (\cF -> 
    do
        bf    <- currentInContext cF
        normBF <- fmap BFragment . apply transRule . bstats . conv $ bf
        newBF <- applyInNewContext tryRemoveUnneededIfConditionsS normBF
        newF  <- fromContext newBF
        return $ replaceInContext (conv newF) cF
    )
 
-- Full AST Version of tryRemoveUnneededIfConditionsS
-- This version does not take into ccount all versions
-- if (p||x==0) s+=x;
-- if - else problematic , check
removeUnneededIfConditionsS :: Strategy (Context Statement)
removeUnneededIfConditionsS = 
    somewhere (
            useC (checkC isIf) 
        .*. withIfCondition (try $ choice $ useRs splitCombinedE)
        .*. repeat1 (choice $ useRs expandIfWithOr) 
        .*. (   -- check cond1
            useC tryMakeEmptyIf
              .|.
                -- check cond2 in elseif
                withElsePart (
                  useC tryMakeEmptyIfNested
                 {-  .|.
                      -- check cond3 in elseif-elseif
                      withNestedIfElsePart ifCPStratNested'
                )
          )
        .*. try (useR combineIfConditionsRule) -- combine remaining conditions-}
        ))
    )

tryMakeEmptyIf :: Rule (Context Statement)
tryMakeEmptyIf =  makeRule "tryMakeEmptyIf" (\cS -> 
    do
        s@(IfElse _ _ f) <- currentInContext cS
        guard $ applicable ifCPStratTopLevel . newTermContext $ head (conv s) -- [BStat], if/ifelse always [s]
        return $ replaceInContext f cS -- true-part was useless
    )

tryMakeEmptyIfNested :: Rule (Context Statement)
tryMakeEmptyIfNested =  makeRule "tryMakeEmptyIfNested" (\cS -> 
    do
        s@(If _ _) <- currentInContext cS
        guard $ applicable ifCPStratNested . newTermContext $ head (conv s) -- [BStat], if/ifelse always [s]
        return $ replaceInContext Empty cS -- this c was useless
    )

norm :: (IsProgram p, IsTerm p) => Strategy (Context p)
norm = 
         useC cleanBoolExp --  .*. useC normBoolExp
     .*. useC cleanArithExp .*. useC normArithExp
     .*. replicateS 3 run
     .*. check (\cp -> (\_ -> True) . fromJust . fromContext $ cp)
     .*. option (outermost . useR $ pushIfInFor)
     -- .*. try (outermost . use $ forToForEachRule) -- !
     .*. option (outermost . use $ replaceContinue)
    where
        run :: (IsProgram p, IsTerm p) => Strategy (Context p)
        run = cleanStructure
            -- always do - if
            .*. outermost (
                        useR removeRedundantIfElse
                    |>  choice (useRs removeUselessIf) 
                    |>  choice (useRs removeUselessIfElse) 
                    |>  choice (useRs removeUselessCheck)
                    |>  choice (useRs simplifyIfs)
                )
            .*. outermost (
                    choice (useRs [removeEmptyIf, removeEmptyElse]) 
                )
            .*. outermost (useR reverseNegIfElse)
            .*. outermost (choice $ useRs collapseIf)
            -- exp
            -- .*. useC normBoolExp
            .*.  (outermost (use . choice . rs $ simplifyComposed))
            
-- never reappear
cleanBoolExp :: Strategy (Context Expression)
cleanBoolExp = outermost . use . choice . rs $ [equalsTrue, equalsFalse]

cleanArithExp :: Strategy (Context Expression)
cleanArithExp = outermost . use . choice . rs $ replaceCompoundOp

normArithExp :: Strategy (Context Statement)
normArithExp = outermost . use . choice . rs $ [operandsOrder]




