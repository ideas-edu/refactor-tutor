------------------------------------------------------------------
-- 
--
-- Created: 11-5-2018
-- CNF/DNF from logic tutor
------------------------------------------------------------------

module Domain.Refactoring.ExpStrategy where

import Ideas.Common.Library as Ideas
import Domain.Syntax as IP
import Domain.Refactoring.Rules.Rules
import Domain.Refactoring.Util
import Control.Monad

import Domain.Algebra.Boolean
import Domain.Logic.Utils hiding (somewhereOr, somewhereAnd)


-- DNF
normBoolExp :: LabeledStrategy (Context Expression)
normBoolExp = label "DNF" $ repeatS $
   orRules .|. somewhereOr (nnfStep |> distrAnd') 

-- A specialized variant of the somewhere traversal combinator. Apply
-- the strategy only at (top-level) disjuncts
somewhereOr :: IsStrategy g => g (Context Expression) -> Strategy (Context Expression)
somewhereOr = somewhereWhen $ \a -> 
   case currentInContext a of
      Just (Infixed OR _ _) -> False
      _                     -> True

somewhereAnd :: IsStrategy g => g (Context Expression) -> Strategy (Context Expression)
somewhereAnd = somewhereWhen $ \a -> 
   case currentInContext a of
      Just (Infixed AND _ _) -> False
      _                      -> True

orRules :: LabeledStrategy (Context Expression)
orRules = label "OrRules" . useRules . rs $
   simplifyOr ++ [idemOr, absorbOr]

andRules :: LabeledStrategy (Context Expression)
andRules = label "AndRules" . useRules .rs $
   simplifyAnd ++ [idemAnd, absorbAnd]

-- A step towards Negation Normal Form (NNF)
nnfStep :: Strategy (Context Expression)
nnfStep =   
        simplifyStep
    -- |>  (remove specialGroupLiterals ./. specialDistrNot ./. specialDeMorganNot)
    |>  deMorgan

simplifyStep :: LabeledStrategy (Context Expression)
simplifyStep = label "Simplify" $ oncetdPref $
   orRules .|. andRules .|.
   useRules (rs [notTrue, notFalse, doubleNeg])

deMorgan :: LabeledStrategy (Context Expression)
deMorgan = label "DeMorgan" $ somewhere $
        liftToContext (rr deMorganOr) -- liftToContext generalRuleDeMorganOr ./. 
   .|.  liftToContext (rr deMorganAnd) -- liftToContext generalRuleDeMorganAnd ./.

distrAnd' :: LabeledStrategy (Context Expression)
distrAnd' = label "DistrAnd" $ oncebuPref $
   useRules . rs $ distrAnd -- liftToContext generalRuleDistrAnd ./. 

useRules :: [Rule Expression] -> Strategy (Context Expression)
useRules = alternatives . map liftToContext

-- specialization of De Morgan rules with a not inside (gives higher priority)
specialDeMorganNot :: LabeledStrategy (Context Expression)
specialDeMorganNot = label "" succeed 

specialGroupLiterals :: LabeledStrategy (Context Expression)
specialGroupLiterals = label "SpecialSort" $ somewhere $
   liftToContext groupLiteralsOr .|. liftToContext groupLiteralsAnd

-- p \/ q \/ ~p  ~> reorder p and ~p
-- p \/ q \/ p   ~> reorder p's
groupLiteralsOr :: Rule Expression
groupLiteralsOr = siblingOf groupCommutativity $ ruleMaybe "ComplOr.sort" $ \p -> do
   let xs = disjunctions p
       ys = sortLiterals xs
   guard (xs /= ys)
   return (ors ys)

-- p /\ q /\ ~p  ~> reorder p and ~p
-- p /\ q /\ p   ~> reorder p's
groupLiteralsAnd :: Rule Expression
groupLiteralsAnd = idRule ""

sortLiterals :: [Expression] -> [Expression]
sortLiterals = id

instance Boolean Expression where
    (<&&>) = Infixed AND
    (<||>) = Infixed OR
    complement = Prefixed IP.Not

instance BoolValue Expression where
    isTrue (LiteralExpr (BoolLiteral True))   = True
    isTrue _                                  = False

    isFalse (LiteralExpr (BoolLiteral False)) = True
    isFalse _                                 = False

instance CoBoolean Expression where
   isAnd (Infixed AND p q)       = Just (p, q)
   isAnd _                       = Nothing
   isOr  (Infixed OR p q)        = Just (p, q)
   isOr  _                       = Nothing
   isComplement (Prefixed IP.Not p) = Just p
   isComplement _                = Nothing