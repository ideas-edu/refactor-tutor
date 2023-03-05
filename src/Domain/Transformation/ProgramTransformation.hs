------------------------------------------------------------------
-- Transformation
--
-- Created: 9-9-13, modified x-x-x 
--
------------------------------------------------------------------

module Domain.Transformation.ProgramTransformation where

import Domain.Syntax
import Domain.Dependency
import Domain.Views
import Data.Maybe
import Data.List
import Data.Monoid
import qualified Data.Sequence as Sq
import Data.Generics.Uniplate.DataOnly (universeBi, transformBi, transform)
import Control.Monad
import qualified Data.Map as Map
import Utils.Utils

import Ideas.Common.View
--------------------------------------------------------------------------------
-- Program level

-- not used anywhere, remove?
cleanProgram :: Program -> Program
cleanProgram = resetLocations . transformBi removeAnnotations 

-- | Normalisations before extracting a strategy    
prepareModel :: Program -> Program
prepareModel p = 
    let
        -- Perform only once
        finalise = id
        
        transformations = 
               transformBi separateVarDeclsS
            .   transformBi separateVarInitandDeclS
               
    in finalise $ loopTransformations transformations p      

prepareModels :: Fragment -> Fragment
prepareModels p = 
    let
        transformations = 
               transformBi separateVarDeclsS
            .   transformBi separateVarInitandDecl2           
    in transformations p   

-- | Create block inside all for/while/if's 
-- | if () stat; -> if () { stat; }
createBlock :: Statement -> Statement
createBlock stat = case stat of
    If e s             -> If e (s2block s)
    For a b c s        -> For a b c (s2block s)
    While a s          -> While a (s2block s)
    IfElse e s1 s2     -> IfElse e (s2block s1) (s2block s2)
    ForEach dt idt e s -> ForEach dt idt e (s2block s)
    _                  -> stat
    where
        s2block b@(Block _ _) = b
        s2block Empty         = Block 0 []   
        s2block s             = Block 0 [s]         
                
--------------------------------------------------------------------------------
-- Variable declaration and initialisation, constants

-- TODO code verbetern
replaceConstants:: Program -> Program
replaceConstants p = p { body = replaceAllC findConstants }
    where
        findConstants:: Map.Map Identifier Expression
        findConstants = Map.fromList $ filter (\(id, expr) -> 1 == nrOfOccurences id) allAssigns
        
        allAssigns = [ (id, expr) | s@(ExprStat (Assignment Assign (IdExpr id) expr)) <- universeBi p] -- [Stat]
        
        allIdentifiers = changesIds p 
        -- hoe vaak komen ze voor?
        nrOfOccurences id = length $ filter (==id) allIdentifiers-- foldl (\a b -> if b == id then a + 1 else a) 0 allIdentifiers

        replaceAllC cs = (transform t) $ body p
            where
                t r@(ExprStat(Assignment Assign (IdExpr i) expr)) = 
                    if Map.member i cs then Empty 
                    else ExprStat (Assignment Assign (IdExpr i) (transform t' expr))                         
                t x = transformBi t' x
             
                t' (IdExpr x) = if Map.member x cs then fromJust (Map.lookup x cs) else (IdExpr x)
                t' x = x 

-- remove declaration of unused vars
-- Might occur after copy prop
removeUnusedDecl :: Program -> Program
removeUnusedDecl p = undefined -- transformBi t p
    where
        toRemove :: [Identifier]
        toRemove = concatMap getDeclNoInit (getStats p)     
        
        t stat = undefined  

-- | Put every variable assignment in a seperate statement 
separateVarDeclsS:: Statement -> Statement
separateVarDeclsS = insertStatsS separateVarDecls'
    where
        -- int x = 1, y = 2; -> int x = 1; int y = 2;
        separateVarDecls':: Statement -> [Statement]
        separateVarDecls' stat = 
            case stat of
                VarDeclarations _ [_]    -> [stat]
                VarDeclarations dt decls -> map (\d -> VarDeclarations dt [d]) decls
                _ -> [stat]

-- | ?
separateVarInitandDeclS :: Statement -> Statement
separateVarInitandDeclS = insertStatsS sep'

-- | ?
separateVarInitandDecl2:: Fragment -> Fragment
separateVarInitandDecl2 = mapP (insertStatsWith sep')

-- | Possibly splits a var init+decl
sep':: Statement -> [Statement]    
sep' stat@(VarDeclarations dt [Assignment op l r]) = 
    if isArrType dt 
        then [stat] 
        else [VarDeclarations dt [l], ExprStat (Assignment op l r)]
sep' stat = [stat]

--------------------------------------------------------------------------------
-- Loops

while2for' :: (Statement, Statement, Expression) -> Maybe Statement 
while2for' (init, stat, incr) = case stat of
    While cond stats -> fmap (\i -> For i [cond] [incr] stats) (statToForInit init)
    _                -> Nothing


-- if a pattern is recognised that is easily ported to a for, change it
while2for :: Statement -> Statement      
while2for stat =
    case stat of
        Block nr stats -> Block nr $ update stats
            where 
                whileLocations = findIndices isWhile stats
                getWhileElements stats index = 
                    do
                        let (While e (Block _ body)) = stats!!index
                        
                        -- is there a statement before the while          
                        initIndex <- if index == 0 then Nothing else Just $ index - 1
                        -- does it assigns a value to some counter
                        unless (isVarAssign $ stats!!initIndex) Nothing
                        
                        -- which var is used for looping?
                        loopId <- 
                            if length (changesIds $ stats!!initIndex) == 1 -- niet x=y=0..
                                then Just $ head (changesIds $ stats!!initIndex)
                                else Nothing
                                
                        -- is this var used in the while-condition?
                        unless (loopId `elem` usesIds e) Nothing
                        
                        -- is there a stat?
                        when (null body) Nothing
                        
                        -- does it changes the counter var?
                        unless (loopId `elem `changesIds (last body)) Nothing
                    
                        return (initIndex, index, last body, init body)
                        
                we:: [(Int, Int, Statement, [Statement])]        
                we = mapMaybe (getWhileElements stats) whileLocations
                
                -- TODO initdecls
                makeFor (initIndex, index, counter, body) = 
                    For (ForInitExpr [toExpr $ stats!!initIndex]) [getCondition $ stats!!index]
                        [toExpr counter] (Block 0 body)
                
                removeWhile (initIndex, _, _, _) stats = 
                    let
                        (beforeWhile, init:while:rest) = splitAt initIndex stats
                    in beforeWhile ++ rest
                    
                insertFor loc forStat stats=
                    let
                        (before, after) = splitAt loc stats
                    in before ++ forStat:after
                
                foundWhile = we
                -- remove while and insert for
                replace stats whileInfo@(a,_,_,_) = 
                    let
                        forStat = makeFor whileInfo
                    in (insertFor a forStat . removeWhile whileInfo) stats
                update stats = foldl replace stats foundWhile
                
                isWhile st = case st of
                    While _ _ -> True
                    _ -> False
                isVarAssign st = case st of
                    ExprStat Assignment {} -> True --changesIds st == [id]
                    VarDeclarations {} -> True -- changesIds st == [id]
                    _ -> False
                
                getCondition (While e _) = e
                
                toExpr (ExprStat e) = e
                toExpr (VarDeclarations _ e) = head e -- head!!
               
        _ -> stat   


-- | Convert for to while.
-- Creates a new scope for the loop variables
-- TODO use views
for2while :: Statement -> Maybe Statement
for2while stat = case stat of
    For inits conds incrs stats ->
        let inits'    = forInitToStat inits

            -- Java: only one cond, PHP: last is used, rest is evaluated
            (conds', toBody') = case conds of
                        [] -> (trueLt    , [])
                        xs -> (last conds, map ExprStat $ init xs)

            incrs'    = map ExprStat incrs

            -- remove, other rule
            stats' = case stats of 
                (Block i xs) -> Block i (toBody' ++ xs ++ incrs')
                singleStat   -> if null (toBody' ++ incrs') 
                                        then singleStat
                                        else makeBlock (toBody' ++ [singleStat] ++ incrs')   

            while = fromJust $ insertStatBeforeContinueInWhile incrs' (While conds' stats')

        in Just (makeBlock $ inits' ++ [while])
    _ -> Nothing 

-- Insert a statement before each continue in a while, except in nested loops
insertStatBeforeContinueInWhile :: [Statement] -> Statement -> Maybe Statement
insertStatBeforeContinueInWhile stats (While e b) = Just $ While e (isbc b)
    where
        isbc (Block i xs)     = Block i (isbcList [] xs)
        isbc (If e b)         = If e (isbc b)
        isbc (IfElse e t f)   = IfElse e (isbc t) (isbc f)
        isbc Continue         = makeBlock (stats ++ [Continue])
        isbc x                = x -- nested loops are left alone
        
        isbcList :: [Statement] -> [Statement] -> [Statement]
        isbcList before [] = before
        isbcList before (curr:after) = case curr of
            Continue -> isbcList (before ++ stats ++ [Continue]) after
            _        -> isbcList (before ++ [isbc curr]) after
insertStatBeforeContinueinWhile _ _ = Nothing

-- No support for collections (leads to incorrect rewrite)
foreachToFor :: Statement -> Maybe Statement
foreachToFor fe@(ForEach dt i e s) = 
    let counterVar  = IdExpr (prependIdt "c_" i)

        arrayIdt = case e of
            (IdExpr   ie)        -> ie
            (ArrayAcc idt _)     -> prependIdt "tmp_" idt
            (Call     idt _)     -> prependIdt "tmp_" idt
            (Property idt1 idt2) -> prependIdt "tmp_" (idt1 <> idt2)
            (NewArray _ _)       -> makeIdentifier "tmp_array"
            _                    -> makeIdentifier "tmp_array_unknown"

        -- In some cases a temp var is needed to store the array
        tempArrayVar = if isIdt e
            then Empty
            else ArrayDecls (ArrayType dt) (ArrInit arrayIdt e)

        for = For 
            (ForInitDecls IntType [counterVar .=. 0])
            [counterVar .<. Property arrayIdt (makeIdentifier "length")]
            [(.++.) counterVar]
                (VarDeclarations dt [IdExpr i .=. ArrayAcc arrayIdt counterVar] <> s)

    in Just (tempArrayVar <> for)
foreachToFor _                  = Nothing

-- for(int i = 0; i < a.length; i++) f(a[i]); --> for(int i : a) f(i);
-- Map is needed to find the type of the array that is looped over.
-- Functions called that change the array are not taken into account.
forToForeach :: ([(Identifier, DataType)], Statement) -> Maybe Statement
forToForeach (dtMap, s) = case s of
        -- should ensure first to last
        For forinit [cond] [incr] body -> do
            c            <- match viewInitZero forinit
            (c2, arrIdt) <- match viewToArrLength cond
            c3           <- match viewIncrOne incr
            dt           <- lookup arrIdt dtMap

            if c == c2 && c2 == c3                -- same counter? TODO replace by matcher
                && not (changesId c body)         -- counter is never updated
                && counterNotNeeded body arrIdt c -- counter is not used for other purposes than accessing index
                && not (changesId arrIdt body)    -- array is not modified
                && not (hasBranching body)        -- no breaks etc in body
                then Just $ forToForeach' dt arrIdt body
                else Nothing

        _ -> Nothing
    where
        counterNotNeeded body arrIdt c    = usesCounterAsArrIdx body arrIdt c == usesCounter body c   
        usesCounterAsArrIdx body arrIdt c = length [ 1 | ArrayAcc idt (IdExpr idt2) <- universeBi body, idt == arrIdt, idt2 == c]
        usesCounter body c                = length [ 1 | IdExpr idt <- universeBi body, idt == c]

forToForeach' :: DataType -> Identifier -> Statement -> Statement
forToForeach' dt arrIdt body = ForEach dt arrItVar (IdExpr arrIdt) (replaceArrayIndexer arrIdt arrItVar body)
    where
        arrItVar = makeIdentifier "tmp_arrIt"
        replaceArrayIndexer from to = transformBi (f' from to)
        f' from to (ArrayAcc idt ex) | idt == from = IdExpr to
        f' _ _ x = x
        

-- Returns nothing if nothing changes
incrOneRule :: Expression -> Maybe Expression
incrOneRule e = do 
    e' <- build viewIncrOne <$> match viewIncrOne e
    if e'==e then Nothing else Just e'





-- | Are there any breaks, continues or returns in the program?
hasBranching :: IsProgram p => p -> Bool
hasBranching p = not . null $ (breaks ++ continues ++ returns)
    where 
        breaks    = [ 1 | Break    <- universeBi stats] 
        continues = [ 1 | Continue <- universeBi stats] 
        returns   = [ 1 | Return _ <- universeBi stats] 
        stats     = toP p

--------------------------------------------------------------------------------
-- Meta data

-- | Remove #feedback and #mustuse annotations, but not #alt
removeAnnotations :: Statement -> Statement
removeAnnotations stat = case stat of
    Feedback _ stat -> stat  
    MustUse stat    -> stat        
    _               -> stat   

-- | Replace all location numbers by 0
resetLocations :: Program -> Program
resetLocations = transformBi f . transformBi g . transformBi h . transformBi i . transformBi j
    where
        f (FHole _)     = FHole 0 
        f x             = x 
        g (HoleExpr _)  = HoleExpr 0 
        g x             = x  
        h (Block _ s)   = Block 0 s
        h x             = x 
        i (PHole _ )    = PHole 0
        i x             = x 
        j Fragment { stats = xs } = Fragment { stats = xs, fLocId = 0 }
        j x             = x

--------------------------------------------------------------------------------
-- Utils

class ExpandTrans b where
    expandTrans:: (b -> [b]) -> b -> b

instance ExpandTrans Statement where
    expandTrans transformation (Block nr stats) = Block nr $ foldl (\xs s -> xs ++ transformation s) [] stats  
    expandTrans _ stat = stat

         
insertStatsS:: (Statement -> [Statement]) -> Statement -> Statement
insertStatsS transformation (Block n stats) = Block n $ foldl (\xs s -> xs ++ transformation s) [] stats  
insertStatsS _ stat = stat

-- | Apply a transformation that results in a list of stats and merge stats    
insertStatsWith:: (Statement -> [Statement]) -> [Statement] -> [Statement]
insertStatsWith f = foldl (\xs s -> xs ++ f s) []  
            

            