{-# LANGUAGE FlexibleContexts #-}

module Domain.Base.Transformations 
where

import Domain.Syntax hiding (Statement(..), Expression(..), ForInit(..), (.-.), AssignOp(..))
import Domain.Dependency
import Domain.Base.AST as B
import Domain.Refactoring.Util

import Utils.Utils

import Ideas.Common.Environment
import Ideas.Common.Library

import Data.Data
import Data.Maybe
import Data.List
import qualified Data.Sequence as Sq
import Data.Generics.Uniplate.DataOnly (transformBi, transformBiM)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
--import Data.Map.Merge.Strict
import Domain.Base.ExprTransformations
import Data.Foldable (toList)

-- disjoint from Data.Set not supported on Ideas server
setDisjoint :: Ord a => Set.Set a -> Set.Set a -> Bool
setDisjoint s1 s2 = Set.null (s1 `Set.intersection` s2)
--------------------------------------------------------------------------------
-- Program level

removeEmpties :: [BStat] -> [BStat]
removeEmpties = filter keep
    where
        keep (IfElse _ [] []) = False -- condition with assignment?
        keep (Block [])       = False
        keep (While _ [])     = False
        keep (Assignment a b) | a == b = False
        keep _                = True

clean :: BStat -> BStat
clean (IfElse c [] f@(_:_)) = IfElse ((!.) c) f [] -- Block f
clean (Block [s])         = s
clean (While c [Block s]) = While c s
clean (IfElse c [Block t] f)  = IfElse c t f
clean (IfElse c t [Block f])  = IfElse c t f
clean x                   = x

-- Removing dead code --------------------------------------------------

-- remove statements after break/continue/return
-- TODO

-- Removing unused variables --------------------------------------------------

removeUnusedVars :: BProgram -> BProgram
removeUnusedVars p = case p of 
    B.Program {}   -> p { bbody = snd . ruv Set.empty [] $ bbody p }
    B.OOProgram cs -> transformBi removeUnusedVarsF p

removeUnusedVarsF :: BFragment -> BFragment
removeUnusedVarsF = mapBFragment (snd . ruv Set.empty [] )

ruv :: Set.Set Identifier -> [BStat] -> [BStat] -> (Set.Set Identifier, [BStat])
ruv neededIdts remStats [] = (neededIdts, remStats)
ruv neededIdts remStats stats = case last stats of
    Block      xs       -> let (newIdts, newXs) = ruv neededIdts [] xs
                           in ruv newIdts (Block newXs:remStats) (init stats) 
    IfElse     c t f    -> let (idtsT, ruvT) = ruv neededIdts [] t
                               (idtsF, ruvF) = ruv neededIdts [] f
                               idtsC         = usesIdsSet c
                           in changeStat (Set.unions [idtsT, idtsF, idtsC]) (IfElse c ruvT ruvF)
    While      c body   -> let idtsWhile          = Set.unions [neededIdts, usesIdsSet body, usesIdsSet c]
                               (newIdts, ruvBody) = ruv idtsWhile [] body
                           in changeStat newIdts (While c ruvBody)
    Print      e        -> keepStat (neededIdts `Set.union` usesIdsSet e) 
    ExprStat   e        -> keepStat (neededIdts `Set.union` usesIdsSet e) 
    Break               -> keepStat neededIdts
    Continue            -> keepStat neededIdts
    Return     me       -> keepStat (neededIdts `Set.union` usesIdsSet me)
    Assignment l r      -> let idtsR   = usesIdsSet r `Set.union` neededIdts
                               changes = Set.fromList $ changesIds (last stats)
                           in if setDisjoint changes neededIdts
                             then dropStat neededIdts
                             else keepStat ((neededIdts Set.\\ changes) `Set.union` usesIdsSet r)
    where
        keepStat idts        = ruv idts (last stats:remStats) (init stats)
        changeStat idts stat = ruv idts (stat:remStats) (init stats)
        dropStat idts        = ruv idts remStats (init stats)

        usesIdsSet :: FindIds a => a -> Set.Set Identifier
        usesIdsSet           = Set.fromList . usesIds

--------------------------------------------------------------------------------
-- Variable declaration and initialisation, constants

-- | renames all variables to v1, v2, ..
renameVars :: BProgram -> (BProgram, Environment)
renameVars p = (renamedProgram, env)
    where 
        (renamedProgram, (_, varMap)) = runState (renameVars' p) (1, Map.empty) 
        env = insertRef (makeRef "varMap") (Map.toList varMap) $ makeEnvironment [] 

        renameVars' :: BProgram -> State (Int, Map.Map String String) BProgram
        renameVars' p = transformBiM renameParam p 
            >>= transformBiM rename -- volgorde maakt uit!!
            
            where
                rename el = case el of     
                    IdExpr idt      -> IdExpr   <$> rename' idt
                    ArrayAcc idt e  -> ArrayAcc <$> rename' idt <*> pure e
                    Property idt i  -> Property <$> rename' idt <*> pure i       
                    _               -> return el 
                
                renameParam (Param dt idt) = Param dt <$> rename' idt
                renameParam p              = return p

                rename' (Identifier i) = 
                    do
                        (nr, map) <- get
                        let newName = if Map.member i map then map Map.! i else 'v': show nr
                            newEnv  = Map.insert i newName map
                            newNr   = if Map.member i map then nr else nr + 1
                        put (newNr, newEnv)
                        return $ Identifier newName

-- Renaming a variable, works within 1 method, does not take scope into account
renameVar :: Data.Data.Data a => Identifier -> Identifier -> a -> a
renameVar from to = transformBi (renameParam from to) . transformBi (renameVarE from to)

renameVarE :: Identifier -> Identifier -> BExpr -> BExpr
renameVarE from to e = case e of     
    IdExpr idt     | idt == from -> IdExpr to
    ArrayAcc idt e | idt == from -> ArrayAcc to e      
    _                            -> e

renameParam :: Identifier -> Identifier -> Param -> Param
renameParam from to p = case p of     
    Param dt idt    | idt == from -> Param dt to     
    _                             -> p

renameVarFromMap :: IdtRenameMap -> BExpr -> BExpr
renameVarFromMap rMap e = case e of     
    IdExpr idt     | idt `Map.member` rMap -> IdExpr (rMap Map.! idt)
    ArrayAcc idt e | idt `Map.member` rMap -> ArrayAcc (rMap Map.! idt) e      
    _                                      -> e

renameVarsWithMap :: Data.Data.Data a => Map.Map Identifier Identifier -> a -> a
renameVarsWithMap rMap = transformBi (renameVarFromMap rMap)

type IdtCountMap  = Map.Map Identifier Int
type IdtRenameMap = Map.Map Identifier Identifier

-- | RenameRedefinedVars, x=1;x=2 -> x=1, x_1=2
-- Does not work for arrays

-- TODO rewrite this and copyProp on BFragment/Bprogram/BStats
renameRedefinedVars :: BProgram -> BProgram
renameRedefinedVars p = case p of 
    B.Program {}   -> p { bbody = start (bbody p) }
    B.OOProgram cs -> transformBi (mapBFragment start) p
    where 
        start = snd . mapAccumL rdvS Map.empty 

-- Map a function on all 'fragments' (all methods or )
mapBStats :: ([BStat] -> [BStat]) -> BProgram -> BProgram
mapBStats f p = case p of 
    B.Program {}   -> p { bbody = f (bbody p) }
    B.OOProgram cs -> transformBi (mapBFragment f) p

rdvL :: IdtCountMap -> [BStat] -> (IdtCountMap, [BStat])
rdvL = mapAccumL rdvS

rdvS :: IdtCountMap -> BStat -> (IdtCountMap, BStat)
rdvS icMap stat = case stat of
    Assignment (IdExpr l) r -> 
                           let r'           = rdvE icMap r
                               redefinedVar = true -- l `Map.member` icMap
                               newMap       = if redefinedVar
                                                then addOrIncIdt l icMap
                                                else icMap
                               l'           = rdvE newMap (IdExpr l) 
                           in (newMap, Assignment l' r')
    -- TODO array support
    Assignment l r      -> (icMap, Assignment l (rdvE icMap r))

    Block      xs       -> let (newMap, newXs) = mapAccumL rdvS icMap xs in (newMap, Block newXs)
    Return     me       -> (icMap, Return $ fmap (rdvE icMap) me)
    
    IfElse     c t f    -> renameOnly
    While      c xs     -> renameOnly
    Print      e        -> renameOnly
    ExprStat   e        -> renameOnly
    Break               -> continue
    Continue            -> continue
    
    where
        renameOnly = (icMap, renameVarsWithMap (toRenameMap icMap) stat)
        continue   = (icMap, stat) 

rdvE :: IdtCountMap -> BExpr -> BExpr
rdvE m e = case e of
    IdExpr i            -> IdExpr (renameIdtIfFound i m)
    -- TODO not complete for arrays
    ArrayAcc i idx      -> ArrayAcc i (rdvE' idx) 
    Infixed op e1 e2    -> Infixed op (rdvE' e1) (rdvE' e2)
    Prefixed op e       -> Prefixed op (rdvE' e)
    Postfixed op e      -> Postfixed op (rdvE' e)
    Call i ps           -> Call i $ map rdvE' ps
    NewArray dt e       -> NewArray dt (rdvE' e)
    Property i prop     -> Property (renameIdtIfFound i m) prop
    -- Unchanged
    LitExpr l           -> e
    where
        rdvE' = rdvE m

-- If in map (x -> 3) rename to x_3
renameIdtIfFound :: Identifier -> IdtCountMap -> Identifier
renameIdtIfFound idt = maybe idt (\cnt -> appendIdt ("_" ++ show cnt) idt) . Map.lookup idt 

addOrIncIdt :: Identifier -> IdtCountMap -> IdtCountMap
addOrIncIdt idt = Map.insertWith (+) idt 1

-- From (x -> 3, y -> 4) to (x -> x_3, y -> x_4)
toRenameMap :: IdtCountMap -> Map.Map Identifier Identifier
toRenameMap = Map.mapWithKey (\idt cnt -> appendIdt ("_" ++ show cnt) idt) 

--------------------------------------------------------------------------------
-- Copy propagation

type VarMap = Map.Map BExpr BExpr
data CPState = CPState VarMap deriving Show

emptyCPState :: CPState
emptyCPState = CPState Map.empty

mapCPS :: (VarMap -> VarMap) -> CPState -> CPState
mapCPS f (CPState cps) = CPState (f cps)

updateCPState :: BExpr -> BExpr -> CPState -> CPState
updateCPState k v = mapCPS (Map.insert k v)

replaceIfFound :: CPState -> BExpr -> BExpr
replaceIfFound (CPState stateMap) k = fromMaybe k (Map.lookup k stateMap)

-- | Combine two states, keeping only vars from both maps that have the same value
mergeCPState :: CPState -> CPState -> CPState
mergeCPState (CPState s1) (CPState s2) = 
    CPState (Map.mergeWithKey (\k a b -> if a == b then Just a else Nothing) (const Map.empty) (const Map.empty) s1 s2)
    -- TODO use merge

-- | Remove all vars with given identifiers from map
removeFromCPState :: [Identifier] -> CPState -> CPState
removeFromCPState idt = mapCPS (\s -> s Map.\\ Map.fromList (map (\i -> (IdExpr i, lit0 :: BExpr)) idt)  )

-- | Perform copy propagation in a list of statements on the first temporary variable
copyProp :: BProgram -> BProgram
copyProp p = case p of 
    B.Program {}   -> p { bbody = startCP (bbody p) }
    B.OOProgram cs -> transformBi cpFrag p
    where 
        cpFrag :: BFragment -> BFragment
        cpFrag f = f { bstats = startCP (bstats f) } 
        startCP = snd . cpL emptyCPState

cpL :: CPState -> [BStat] -> (CPState, [BStat])
cpL = mapAccumL cp

cp :: CPState -> BStat -> (CPState, BStat)
cp cps stat = case stat of
    Block      xs       -> let (newCPS, newXs) = cpL cps xs in (newCPS, Block newXs)
    IfElse     c t f    -> let (cpsT, newT) = cpL cps t
                               (cpsF, newF) = cpL cps f
                               (_, newC)    = cpe cps c
                               newCPS = mergeCPState cpsT cpsF
                            in (newCPS, IfElse newC newT newF)
    While      c xs     -> let (_, newXs) = cpL newCPS xs -- perform cp locally, not saving state
                               newCPS  = removeFromCPState (changesIds xs) cps
                           in (newCPS, While c newXs)
    Print      e        -> let (newS, newE) = cpe cps e in (cps, Print newE)
    ExprStat   e        -> let (newS, newE) = cpe cps e in (newS, ExprStat newE)
    Break               -> (cps, stat)
    Continue            -> (cps, stat)
    Return     me       -> (cps, Return $ fmap (snd . cpe cps) me)
    Assignment l r      -> let (_, r') = cpe cps r
                               definedWithSelf = notNull $ usesIds stat `intersect` changesIds stat
                               newCPS = if definedWithSelf
                                            then cps  -- Do not store vars defined in itself
                                            else updateCPState l r' cps
                               -- TODO move renaming to separate function
                               maybeRenameL = if definedWithSelf
                                                then makeIdt "_old"
                                                else l
                           in (newCPS, Assignment l r') 

cpe :: CPState -> BExpr -> (CPState, BExpr)
cpe cps e = case e of
    IdExpr i            -> (cps, replaceIfFound cps e)
    Infixed op e1 e2    -> (cps, Infixed op (cpeE e1) (cpeE e2))
    Prefixed op e       -> if op `elem` [Minus, Plus, Not]
                                then (cps, Prefixed op (cpeE e))
                                else (cps, Prefixed op e)
    Call i ps           -> (cps, Call i $ map cpeE ps)
    ArrayAcc i idx      -> let e' = replaceIfFound cps e -- first try complete a[i], then i
                           in (cps, if e' == e then ArrayAcc i (cpeE idx) else e')
    NewArray dt e       -> (cps, NewArray dt (cpeE e))
    -- Unchanged
    Postfixed op e      -> (cps, e)
    LitExpr l           -> (cps, e)
    Property i prop     -> (cps, e)
    where
        cpeE = snd . cpe cps


-- | Copy prop on if (x==n)
copyPropIfCondition :: BStat -> Maybe BStat
copyPropIfCondition ife@(IfElse c t f) | isEqLit = Just $ IfElse c newT f
    where 
        maybeEq            = match eqView c 
        isEqLit            = maybe False (\xs -> length xs == 2 && isJust (find isLit xs)) maybeEq
        ([eqLit], [eqVar]) = partition isLit . fromJust $ maybeEq
        (_, newT)          = cpL (updateCPState eqVar eqLit emptyCPState) t
copyPropIfCondition _ = Nothing                   



-- put every variable assignment in a seperate statement 
--separateVarDecls:: BStat -> BStat
--separateVarDecls = insertStats separateVarDecls'
--    where
--        -- int x = 1, y = 2; -> int x = 1; int y = 2;
--        separateVarDecls':: BStat -> [BStat]
--        separateVarDecls' stat = 
--            case stat of
--                BVarDecl _ (_:[]) -> [stat]
--                BVarDecl dt decls -> map (\d -> BVarDecl dt [d]) decls
--                _ -> [stat]

--------------------------------------------------------------------------------
-- Loops

-- | Start loop at 0 for basic loops in the form:
-- for (int v=l; v<s ;v++) ;
-- for (int v=l; v<s ;v=V+1) ;
-- Broken because of for data type: FIX
loopAt0 :: BStat -> BStat
loopAt0 stat = stat 
{-
    case stat of
        For [init] [cond] [incr] stats -> 
            maybe   stat
                    (\(v, l, s) ->
                        For 
                            [] -- old: Assignment v lit0]
                            [Infixed Less v ((.-.) s (makeInt l))] --(LitExpr (IntLiteral (s-l)))
                            [incr] -- [replaceExpr' incr]
                            (replaceExpr' v l stats)
                    )
                    $ check init cond incr
                                   
        _ -> stat
        
    where
        -- replaceExpr' :: Data.Data.Data on => BExpr -> Int -> on -> on
        replaceExpr' varid startLit = replaceExpr varid (incExpr startLit varid)
          
        check :: BExpr -> BExpr -> BExpr -> Maybe (BExpr, Int, BExpr)            
        check a b c = do
            (v, l) <- simpleInit a
            s <- simpleCond v b
            guard $ simpleIncr v c
            return (v, l, s)
         
        -- fix   
        --simpleInit (Assignment varid@(IdExpr _) (LitExpr (IntLiteral startLit)))
        --    | startLit == 0 = Nothing
        --    | otherwise     = Just (varid, startLit)
        simpleInit _ = Nothing
        
        simpleCond var1 (Infixed Less var2 x)-- (LitExpr (IntLiteral i))
            | var1 == var2   = Just x
            | otherwise      = Nothing
                
        simpleCond _ _ = Nothing
        
        -- fix
        --simpleIncr var1 (Prefixed Incr var2) = var1 == var2
        --simpleIncr var1 (Assignment var2 (Infixed Addition _ var3)) = (var1 == var2) && (var2 == var3)
        simpleIncr _ _ = False
            
--        idss :: Statement -> [Identifier]
--        idss (Block _ xs) = concatMap (\s -> case s of 
--            For a b c d -> idss d
--            _ -> usesIds s
--            ) xs
-}

--------------------------------------------------------------------------------
-- Branching

expandIfWithOr :: BStat -> BStat
expandIfWithOr stat = case stat of
    IfElse (Infixed OR p q) t f -> IfElse p t [IfElse q t f]
    _                           -> stat

-- | Inverse of extracting final statements from if and else
ifElseFinally :: [BStat] -> [BStat]
ifElseFinally stats = case stats of
    a:b:[IfElse c t f, finally] -> a:b:[IfElse c (t ++ [finally]) (f ++ [finally])]
    _                           -> stats

simplifyIf :: BStat -> BStat
simplifyIf (IfElse p [IfElse q a [] ] b) | a == b =
    IfElse (Infixed OR q (Prefixed Not p)) a []
simplifyIf (IfElse p a [IfElse q b []] ) | a == b =
    IfElse (Infixed OR p q) a []
simplifyIf s = s

-- remove continue if present at top-level in an if-else
removeContinue :: BStat -> Maybe BStat
removeContinue (While c body) 
    | any continueInIfElse body = Just $ While c (before ++ [tIfElse after ifelse])
    where
        continueInIfElse (IfElse _ t@(_:_) _) | hasContinueTopLevel t = True
        continueInIfElse (IfElse _ _ f@(_:_)) | hasContinueTopLevel f = True
        continueInIfElse _ = False

        hasContinueTopLevel xs = last xs == Continue -- must be last statement

        locIfElseCon = fromJust $ findIndex continueInIfElse body

        (before, ifelse:after) = splitAt locIfElseCon body
   
        tIfElse after (IfElse c t f) 
            | hasContinueTopLevel t = IfElse c (init t) (f ++ after)
            | hasContinueTopLevel f = IfElse c (t ++ after) (init f)
        tIfElse _ _ = error "replaceContinue"  
removeContinue _ = Nothing               

--------------------------------------------------------------------------------
-- Utils

-- replaceExpr:: Data.Data.Data on => BExpr -> BExpr -> on -> on
replaceExpr old new stat = 
    let
        old2new e = if e == old then new else e
    in transformBi old2new stat

-- apply a transformation that results in a list of statements
insertStats:: (BStat -> [BStat]) -> BStat -> BStat
insertStats transformation (Block stats) = Block $ foldl (\xs s -> xs ++ transformation s) [] stats  
insertStats _ stat = stat  