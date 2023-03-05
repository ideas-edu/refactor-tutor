{-# LANGUAGE FlexibleContexts #-}
module Domain.Refactoring.Equality where

import Domain.Syntax
import Domain.Base.Conversion
import Domain.Base.Normalisation
import Domain.Base.AST
import Domain.Refactoring.Util
import Domain.Refactoring.Strategy
import Domain.Evaluator

import Ideas.Common.Library
import Ideas.Service.State
import Ideas.Service.BasicServices (onefirst)
import Data.Maybe
import Data.Either


-- | Are there any steps left?
stepsLeft :: Exercise a -> a -> Bool
stepsLeft ex = isLeft . onefirst . emptyState ex

-- | The similarity relation
(~~) :: (IsProgram p) => p -> p -> Bool
(~~) = (==)  --  `on` (fst . normalisePartial) 

xx :: [TestCase] -> ClassMember -> ClassMember -> Bool
xx testCases old nw = testMethodWithSucceeds (conv nw) testCases 

normalise :: (IsProgram p, IsTerm p) => p -> p 
normalise = fromJust . fromContext . fromJust . applyInNewContext norm

cluster :: (IsProgram p, IsTerm p, Convert p BProgram) => p -> BProgram
cluster = conv . normalise 

clusterCM :: ClassMember -> BClassMember
clusterCM = fst . normaliseCM . normalise 