------------------------------------------------------------------
-- Dependency
--
-- Created: 31-10-13
--

------------------------------------------------------------------
module Domain.Dependency 
(
    Deps, dependsOn, FindIds, usesIds, changesIds, usesId, changesId
)
where

import Domain.Syntax
import Utils.Utils
import qualified Domain.Base.AST as B
import Data.List
import Data.Generics.Uniplate.DataOnly (universe)
           
class Deps a where
    -- | Does the first argument depend on the second? 
    dependsOn :: a -> a -> Bool

-- used as s2;s1 -> s1 dependsOn s2
instance Deps Statement where
    s1 `dependsOn` s2 = 
            -- There is a variable changed in s2 that is used or changed in s1, e.g. x=1;print(x);
            notNull (changesIds s2 `intersect` (changesIds s1 `union` usesIds s1))
            -- There is a variable used in s2 that is changed in s1, e.g. a=x;x=9;
        ||  notNull (usesIds s2 `intersect`  changesIds s1)
            -- Both s1 and s2 contain a print-statement
        ||  (notNull [p | Print p <- universe s1] && notNull [p | Print p <- universe s2])  
        
        ||  s1 == Break || s1 == Continue || s2 == Break || s2 == Continue

instance Deps Class where
    c1 `dependsOn` c2 = False

instance Deps ClassMember where
    m1 `dependsOn` m2 = False

data Dependency = Uses | Changes
          
class FindIds a where
    usesIds      :: a -> [Identifier]    
    changesIds   :: a -> [Identifier]
    dependencies :: a -> [(Identifier, Dependency)]
    usesId, changesId :: Identifier -> a ->  Bool
    
    -- default implementations
    usesIds    a = [ x | (x, Uses)    <- dependencies a ]
    changesIds a = [ x | (x, Changes) <- dependencies a ]
    
    usesId i    = elem i . usesIds 
    changesId i = elem i . changesIds

instance FindIds Program where
    dependencies = dependencies . body
        
instance FindIds Statement where                    
    dependencies stat = case stat of
        Block _ stats           -> dependencies stats
        Print expr              -> dependencies expr
        For ints cnds cnts st   -> dependencies4 ints cnds cnts st
        If expr st              -> dependencies2 expr st
        IfElse expr s1 s2       -> dependencies3 expr s1 s2
        While expr st           -> dependencies2 expr st
        Break                   -> []
        Continue                -> []
        Empty                   -> []
        ExprStat e              -> dependencies e
        VarDeclarations _ expr  -> dependencies expr -- eigenlijk niet mooi voor int x;
        Feedback _ s            -> dependencies s
        MustUse s               -> dependencies s    
        Alt xs                  -> concatMap dependencies xs --TODO too strict!
        ArrayDecls _ _          -> error $ "no def for usesIds ArrayDcls"
        _                       -> error $ "no def for usesIds "  ++ show stat      
    
instance FindIds a => FindIds [a] where
    dependencies     = concatMap dependencies

instance FindIds ForInit where
    dependencies (ForInitDecls _ e) = dependencies e
    dependencies (ForInitExpr e)    = dependencies e
    dependencies (FHole _)          = []

instance FindIds Expression where
    dependencies e = zip (usesIds e) (repeat Uses) ++ zip (changesIds e) (repeat Changes) 
    
    usesIds (Assignment Assign _ e) = usesIds e  
    usesIds (Assignment _ e1 e2)    = usesIds e1 ++ usesIds e2 -- += etc    
    usesIds (Prefixed _ e)          = usesIds e
    usesIds (Postfixed _ e)         = usesIds e    
    usesIds (Infixed _ e1 e2)       = usesIds e1 ++ usesIds e2
    usesIds (IdExpr i)              = [i]
    usesIds (ArrayAcc i e)          = i : usesIds e
    usesIds (Property i _)          = [i]
    usesIds (NewArray _ e)          = usesIds e
    usesIds (Call _ es)             = concatMap usesIds es
    usesIds _                       = []   
    
    changesIds (Prefixed Incr (IdExpr i))      = [i]
    changesIds (Prefixed Decr (IdExpr i))      = [i]
    changesIds Prefixed {}                     = []
    changesIds (Postfixed Incr (IdExpr i))     = [i]
    changesIds (Postfixed Decr (IdExpr i))     = [i]
    changesIds Postfixed {}                    = []
    changesIds (Assignment _ (IdExpr i) e)     = i : changesIds e
    changesIds (Assignment _ (ArrayAcc i _) e) = i : changesIds e
    changesIds _                               = []

--------------------------------------------------------------------------------
-- Base type 

instance FindIds B.BStat where                    
    dependencies stat = case stat of
        B.Block stats             -> dependencies stats
        B.Print expr              -> dependencies expr
        B.Assignment l r          -> zip (changesIds stat) (repeat Changes) ++ zip (usesIds stat) (repeat Uses) 
        B.IfElse expr s1 s2       -> dependencies3 expr s1 s2
        B.While expr st           -> dependencies2 expr st
        B.Break                   -> []
        B.Continue                -> []
        B.ExprStat e              -> dependencies e
        B.Return Nothing          -> []
        B.Return (Just e)         -> dependencies e
        x                         -> error $ "no def for deps: " ++ show x
    
    -- new
    changesIds (B.Assignment (B.IdExpr i) e)     = i : changesIds e
    changesIds (B.Assignment (B.ArrayAcc i isx) e) = i : (changesIds isx ++ changesIds e)
    changesIds _                                 = []

    usesIds (B.Assignment (B.IdExpr i) e)         = usesIds e
    usesIds (B.Assignment (B.ArrayAcc idt idx) e) = usesIds idx ++ usesIds e
    usesIds _                                     = []

instance FindIds B.BExpr where
    dependencies e = zip (usesIds e) (repeat Uses) ++ zip (changesIds e) (repeat Changes) 
    
    -- usesIds (B.Assignment _ e)        = usesIds e   --> moved to bstat 
    usesIds (B.Prefixed _ e)          = usesIds e
    usesIds (B.Postfixed _ e)         = usesIds e    
    usesIds (B.Infixed _ e1 e2)       = usesIds e1 ++ usesIds e2
    usesIds (B.IdExpr i)              = [i]
    usesIds (B.ArrayAcc i e)          = i : usesIds e
    usesIds (B.Property i _)          = [i]
    usesIds (B.NewArray _ e)          = usesIds e
    usesIds (B.Call _ es)             = concatMap usesIds es
    usesIds _                         = []   
    
    changesIds (B.Prefixed Incr (B.IdExpr i))    = [i]
    changesIds (B.Prefixed Decr (B.IdExpr i))    = [i]
    changesIds B.Prefixed {}                     = []
    changesIds (B.Postfixed Incr (B.IdExpr i))   = [i]
    changesIds (B.Postfixed Decr (B.IdExpr i))   = [i]
    changesIds B.Postfixed {}                    = []
    --changesIds (B.Assignment (B.IdExpr i) e)     = i : changesIds e
    --changesIds (B.Assignment (B.ArrayAcc i _) e) = i : changesIds e
    changesIds _                                 = []

instance FindIds B.BFragment where
    dependencies = dependencies . B.bstats

instance FindIds B.BClassMember where
    dependencies cm = case cm of
        B.Attribute _ _ exps        -> dependencies exps
        B.Constructor _ _ _ body    -> dependencies body
        B.BMethod {}                -> dependencies (B.mBody cm)

instance FindIds a => FindIds (Maybe a) where
    dependencies a = zip (usesIds a) (repeat Uses) ++ zip (changesIds a) (repeat Changes)
    
    usesIds (Just a) = usesIds a
    usesIds Nothing  = []   
    
    changesIds (Just a) = changesIds a
    changesIds Nothing  = []    

--------------------------------------------------------------------------------
-- Utils

dependencies2:: (FindIds a, FindIds b) => a -> b -> [(Identifier, Dependency)] 
dependencies2 x y = dependencies x ++ dependencies y 

dependencies3:: (FindIds a, FindIds b, FindIds c) => a -> b -> c -> [(Identifier, Dependency)] 
dependencies3 x y z = dependencies2 x y ++ dependencies z

dependencies4:: (FindIds a, FindIds b, FindIds c, FindIds d) => a -> b -> c -> d ->[(Identifier, Dependency)] 
dependencies4 p q r s = dependencies2 p q ++ dependencies2 r s
