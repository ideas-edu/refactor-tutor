{-# LANGUAGE LambdaCase #-}

module Domain.Refactoring.Rules.LoopRules 
(removeLoopByCalc, exitLoopEarly,forToForeachRuleCS,forToWhileStrictS ,replaceBreak,
    pushIfInFor, replaceContinue,storeArrType, foreachToForRule)
where

import Domain.Refactoring.Util
import Domain.Syntax hiding (stats)
import Domain.Transformation.ExprTransformations
import Domain.Transformation.ProgramTransformation
import Domain.Views

import Ideas.Common.Library
import Data.Generics.Uniplate.DataOnly (transformBi)
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import Domain.Transformation.ExprTransformations (simplifyExpr)

-----------------------------------------------------------
-- Loop conversions

-- foreach -> for

foreachToForRule :: Rule Statement
foreachToForRule = makeRule "foreachToFor" foreachToFor

-- for-> while

forToWhileStrictS :: LabeledStrategy (Context Statement)
forToWhileStrictS = label "forToWhileS" $
        useC (checkC isFor)
    .*. (useC (withForInit (checkC isEmptyForInit))
            .|. useC (withForUpdate (checkC isEmptyForUpdate)))
    .*. use for2whileRule
    where
        isEmptyForInit = null . forInitToStat 
        isEmptyForUpdate :: [Expression] -> Bool
        isEmptyForUpdate = null

forToWhileS :: LabeledStrategy (Context Statement)
forToWhileS = label "forToWhileS" $ useC (checkC isFor) .*. use for2whileRule

-- UNUSED
for2whileRule :: Rule Statement
for2whileRule = makeRule "forToWhile" for2while

-- for -> foreach

forToForeachRuleCS :: Rule (Context Statement)
forToForeachRuleCS = makeRule "forToForEach" forToForeachCS
    where
        forToForeachCS :: Context Statement -> Maybe (Context Statement)
        forToForeachCS c = do
            arrTypes  <- arrTypesRef ? environment c
            case currentInContext c of
                Just f@For {} -> (`replaceInContext` c) <$> forToForeach (arrTypes, f)
                _             -> Nothing 

arrTypesRef :: Ref [(Identifier, DataType)]
arrTypesRef = makeRef "arrTypes"

-- To store them in the env
instance Reference DataType
instance Reference Identifier

-- Store the types of all arrays in the fragment
-- TODO: does not work for fragment/blocks not in a method
-- TODO move
storeArrType :: Rule (Context ClassMember)
storeArrType = minor $ makeRule "storeArrType" $ \cc -> do
        cm <- currentInContext cc
        return $ insertRef arrTypesRef (findArrTypes cm) cc
    where
        -- find types and remove 1 layer of ArrayType
        findArrTypes = map (second (fromJust . getArrType)) . findArrDataTypes 

-- UNUSED      
-- | Start loop at 0 for basic loops in the form:
-- for (int v=l; v<s ;v++) ;
-- for (int v=l; v<s ;v=V+1) ;
loopAt0 :: Statement -> Statement
loopAt0 stat = 
    case stat of
        For (ForInitDecls IntType [fInit]) [cond] [incr] stats -> 
            maybe stat
                (\(v, l, s) ->
                    For 
                        (ForInitExpr [Assignment Assign v lit0])
                        [v ..<.. (s ..-.. makeInt l)] 
                        [incr] -- [replaceExpr' incr]
                        (replaceExpr' v l stats)
                )
                $ checks fInit cond incr            
        _ -> stat
        
    where
        -- replaceExpr' :: Data.Data.Data on => BExpr -> Int -> on -> on
        replaceExpr' varid startLit = replaceExpr varid (incExpr startLit varid)
        

        replaceExpr old new s = 
            let old2new e = if e == old then new else e
            in transformBi old2new s

        checks :: Expression -> Expression -> Expression -> Maybe (Expression, Int, Expression)            
        checks a b c = do
            (v, l) <- simpleInit a
            s <- simpleCond v b
            guard $ simpleIncr v c
            return (v, l, s)
            
        simpleInit (Assignment Assign varid@(IdExpr _) (LiteralExpr (IntLiteral startLit)))
            | startLit == 0 = Nothing
            | otherwise     = Just (varid, startLit)
        simpleInit _ = Nothing
        
        simpleCond var1 (Infixed Less var2 x)-- (LitExpr (IntLiteral i))
            | var1 == var2   = Just x
            | otherwise      = Nothing
        simpleCond _ _ = Nothing
        
        simpleIncr var1 (Prefixed Incr var2) = var1 == var2
        simpleIncr var1 (Postfixed Incr var2) = var1 == var2
        simpleIncr var1 (Assignment Assign var2 (Infixed Addition _ var3)) = (var1 == var2) && (var2 == var3)
        simpleIncr _ _ = False

-- Loop -> calc

-- TODO only works for decr            
removeLoopByCalc :: Rule Statement
removeLoopByCalc = makeRule "removeLoopByCalc" $ \s ->
    do
        (a,(_,p), _, ExprStat bodyS) <- match (matchLoopZeroToN matchSingleBlock) s
        (x, n) <- match viewDecrWithN bodyS
        guard (a /= x) -- counter should not be changed in loop
        return $ ExprStat $ IdExpr x .-=. simplifyExpr (Infixed Multiplication (makeInt n) (IdExpr p))

-- Other branching --------------------------------------------------

exitLoopEarly :: LabeledStrategy Statement
exitLoopEarly = label "exitLoopEarly" $ exitLoopEarlyCond .|. exitLoopEarlyBreak 

-- for (;c;) if(!s) { s= true } ~> for (;c && !s;) ..
-- TODO for while
-- TODO when stop-condition is not an idt
exitLoopEarlyCond :: Rule Statement
exitLoopEarlyCond = makeRule "exitLoopEarlyCond" $ \s -> do
    (p, [q], [r], (c,ifB)) <- match (matchForWithBody matchSingleIf) s
    ifStats <- match matchStatList ifB
    s <- match matchNotIdt c -- check for a stop-condition
    let blAssign = mapMaybe (match matchBoolAssign) ifStats -- extract all bool assigns
    guard (any (\(idt, b) -> s == idt && b) blAssign) -- s should be made true in the body
    guard (not $ any (\(idt, b) -> s == idt && not b) blAssign) -- s is never made false again
    return $ For p [q .&&. nt (IdExpr s)] [r] ifB

-- for (;c;) if(!s) { if (x) s= true else f } ~> for (;c && !x ) f
-- Stricter
exitLoopEarlyBreak :: Rule Statement
exitLoopEarlyBreak = makeRule "exitLoopEarlyBreak" $ \s -> do
    (p, [q], [r], (c,ifB)) <- match (matchForWithBody matchSingleIf) s
    s <- match matchNotIdt c -- check for a negated stop-var    
    ifStats <- match matchStatList ifB

    -- check if s is noy made false again
    let falseAssigns = mapMaybe (match (mAssignFalse s)) ifStats -- extract all s=false
    guard (null falseAssigns) -- shouldn't be there

    (stopC, t, f) <- match (matchSingleBlockWith matchId matchIfElse) ifB --if(c) s=bool else ..]

    -- s=true in true- or false-block
    (match (mSingleAssignTrue s) t >> return (For p [q .&&. nt stopC] [r] f))
       <|> (match (mSingleAssignTrue s) f >> return (For p [q .&&. stopC] [r] t))

        where
            mSingleAssignTrue idt = matchSingleBlockWith matchId (mAssignTrue idt)
            mAssignTrue idt = matchAssignWith (matchIdtWith idt) matchTrue
            mAssignFalse idt = matchAssignWith (matchIdtWith idt) matchFalse

matchSingleIf = matchSingleBlockWith matchId matchIf

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

-- simplified, only foreach
replaceContinue :: Rule Statement
replaceContinue = makeRule "replaceContinue" $ \case 
    ForEach dt idt ex (Block loc (If p Continue:stats)) 
      -> Just $ ForEach dt idt ex (If (nt p) $ Block loc stats)
    _ -> Nothing

-- TODO work-in-progress
-- if()break;x++
replaceBreak :: Rule Statement
replaceBreak = makeRule "replaceBreak" $ \s ->
    do
        ((a,b,c,lBody), s') <- match (viewLoopWith matchId matchSingleBlock) s
        ife <- canonical (viewIfElseWith matchId matchBreakInBlock matchId) lBody
                    <|> canonical (viewIfElseWith matchId matchId matchBreakInBlock) lBody
        (b', lBody') <- apply (removeBreak!!0) (b, ife) <|> apply (removeBreak!!1) (b, ife)
        return $ build viewLoop ((a,b',c,lBody'), s')
    where
       -- matchIfWithBreakT = matchIfElseWith matchId matchBreakInBlock matchId
       -- matchIfWithBreakF = matchIfElseWith matchId matchId matchBreakInBlock 
        matchBreakInBlock = matchSingleBlockWith matchId matchBreak

removeBreak :: [RewriteRule (Expression, Statement)]
removeBreak = makeRewriteRules "removeBreak"
    [ \c c1 f -> (c, IfElse c1 Break f)  :~> (c .&&. nt c1, f)
    , \c c1 t -> (c, IfElse c1 t Break)  :~> (c .&&. c1, t)
    ]

pushIfInFor :: RewriteRule Statement
pushIfInFor = makeRewriteRule "pushIfInFor" $  
    \a b c c1 s1 s2 -> IfElse c1 (For a b c s1) (For a b c s2)
                :~> For a b c (IfElse c1 s1 s2)
