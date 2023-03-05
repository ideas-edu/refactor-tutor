{-# LANGUAGE LambdaCase #-}
module Domain.Base.Rules where

import Domain.Base.AST
import Domain.Syntax (InfixOp(..), UnaryOp(..))
import Ideas.Common.Library
import Domain.Refactoring.Util
import Utils.Utils

simplifyIfRule :: [RewriteRule BStat]
simplifyIfRule = makeRewriteRules "simplifyIf" 
    [ \p a q -> IfElse p [IfElse q a []] a  :~> IfElse (Infixed OR (Infixed AND p q) (Prefixed Not p)) a []
    , \p a q -> IfElse p a [IfElse q a []]  :~> IfElse (Infixed OR (Infixed AND p q) (Prefixed Not p)) a []
    ]

-- not always applicable
emptyIfTrueRule :: RewriteRule BStat
emptyIfTrueRule = makeRewriteRule "emptyIfTrue" $
    \p f -> IfElse p [] [f]  :~> f -- !!


combineIfConditionsRule :: RewriteRule BStat
combineIfConditionsRule = makeRewriteRule "combineIfConditionsIf" $
    \p q a b -> IfElse p a [IfElse q a b]  :~> IfElse (Infixed OR p q) a b

-- also for Statement
-- Simplification, only last statement is equal
extractFromIf :: Rule BStat
extractFromIf = makeRule "extractFromIfB" (\case
    IfElse p t f -> 
        if notNull t && notNull f && last t == last f
            then Just (Block [IfElse p (init t) (init f), last t])
            else Nothing
    _ -> Nothing)