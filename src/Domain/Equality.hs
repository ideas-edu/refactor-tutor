{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------
-- Equality
--
-- Created: 9-10-13
------------------------------------------------------------------


module Domain.Equality 
(
    isSolution, solutions, (<==), (*==*), (~~), isPredecessor, (~>), Similar
    , normalise
)

where


import qualified Domain.Syntax as S hiding (Statement(..), Expression(..), ForInit(..))
import Domain.Base.AST
import Domain.Base.Conversion
import Domain.Evaluator
import Domain.Base.Normalisation
import qualified Ideas.Common.Strategy as Ideas
import Ideas.Common.Library
import Data.List
import qualified Data.Map as Map
import Data.Function


type VarList = Map.Map String S.Literal


-- | Is the program a solution according to the strategy?  
isSolution :: Ideas.Strategy S.Program -> S.Program -> Bool
isSolution strat program = any (~~ program) results
    where    
        results = solutions $ liftToContext strat      

-- fromFP
-- | The program that result from following the strategy
--solutions :: Ideas.Strategy (Context Program) -> [Program]
--solutions strat = mapMaybe fromContext $ leafs tree
--    where
--        s    = label "" $ noInterleaving strat
--        pfx  = searchModePrefix isMajorStep eqStep $ emptyPrefix s emptyProgramCtx
--        tree = majorPrefixTree pfx emptyProgramCtx
--        emptyProgramCtx = (newContext . noNavigator) makeEmptyProgram                 

solutions :: S.IsProgram a  => Ideas.Strategy (Context a) -> [a]
solutions strat = [] -- error "Should be fixed"

-- | Is the output of the left program a prefix of the output of the right program
-- Used in for equivalence in Exercise
(<==) :: S.Program -> S.Program -> Bool
prev <==  final = evalProgram prev `eq` evalProgram final
    where
        eq (Right prevRes) (Right finalRes) = prevRes `isPrefixOf` finalRes 
        eq _ _  = True -- error "eval err"       

-- | Is the output of the left program equal to the output of the right program
-- Used for validation
(*==*) :: S.Program -> S.Program -> Bool
(*==*) = (==) `on` evalProgram 


-- | The similarity relation
(~~) :: (S.IsProgram p, ToBase p) => p -> p -> Bool
(~~) = (==) `on` (fst . normalisePartial) 

normalise :: (ToBase a) => a -> BProgram
normalise = fst . normalisePartial

-- | The IsPredecessor relation
isPredecessor :: [String] -> S.Program -> S.Program -> Bool
isPredecessor _ = (~>) `on` (fst . normalisePartial) 

-------------------------------------------------------------------------------
-- Similarity instances

class Eq a => Similar a where
    (~=) :: a -> a -> Bool -- Unused
    (~>) :: a -> a -> Bool  -- ^ is predecessor: x ~> y means x is a predecessor of y (i.e., x can be refined to y)

instance (Similar a, Similar b) => Similar (a, b) where
    (a, b) ~= (p, q) = a ~= p && b ~= q
    (a, b) ~> (p, q) = a ~> p && b ~> q

instance (Similar a, Similar b, Similar c) => Similar (a, b, c) where
    (a, b, c) ~= (p, q, r) = a ~= p && b ~= q && c ~= r
    (a, b, c) ~> (p, q, r) = a ~> p && b ~> q && c ~> r

instance (Similar a, Similar b, Similar c, Similar d) => Similar (a, b, c, d) where
    (a, b, c, d) ~= (p, q, r, s) = a ~= p && b ~= q && c ~= r && d ~= s
    (a, b, c, d) ~> (p, q, r, s) = a ~> p && b ~> q && c ~> r && d ~> s

instance (Similar a) => Similar [a] where
    []     ~= []     = True
    (x:xs) ~= (y:ys) = x ~= y && xs ~= ys
    _xs    ~= _ys    = False 
    
    []     ~> _     = True
    (x:xs) ~> (y:ys) = x ~> y && xs ~> ys
    _xs    ~> []    = False 

instance Similar a => Similar (Maybe a) where
    a ~= b = case (a, b) of
        (Just a', Just b') -> a' ~= b'
        _                -> False
        
    a ~> b = case (a, b) of
        (Just a', Just b') -> a' ~> b'
        (Nothing, _) -> True
        _                -> False
                               
instance Similar BProgram where
    (OOProgram c1) ~= (OOProgram c2) = c1 == c2 
    (Program c1)   ~= (Program c2)   = c1 == c2
    _              ~= _              = False

    (OOProgram c1) ~> (OOProgram c2) = c1 ~> c2
    (Program c1)   ~> (Program c2)   = c1 ~> c2
    _              ~> _              = False

    
instance Similar BStat where
    (Print e1)                  ~= (Print e2)                = e1 ~= e2
    (Block stats1)              ~= (Block stats2)            = stats1 ~= stats2 -- SPECIAL CASE
    (IfElse e1 s11 s12)         ~= (IfElse e2 s21 s22)       = (e1, s11, s12) ~= (e2, s21, s22)  
    (While e1 s1)               ~= (While e2 s2)             = (e1, s1) ~= (e2, s2)
    (ExprStat e1)               ~= (ExprStat e2)             = e1 ~= e2         
    Break                       ~= Break                     = True
    Continue                    ~= Continue                  = True
    Return maybeE1              ~= Return maybeE2            = maybeE1 ~= maybeE2
    Assignment l1 r1            ~= Assignment l2 r2          = (l1, l2) ~= (r1, r2)  
    _                           ~= _                         = False
    
    (Print e1)                  ~> (Print e2)               = e1 ~> e2
    (Block stats1)              ~> (Block stats2)           = stats1 ~> stats2
    (IfElse e1 s11 s12)         ~> (IfElse e2 s21 s22)      = (e1, s11, s12) ~> (e2, s21, s22)  
    (While e1 s1)               ~> (While e2 s2)            = (e1, s1) ~> (e2, s2)
    (ExprStat e1)               ~> (ExprStat e2)            = e1 ~> e2         
    Break                       ~> Break                    = True
    Continue                    ~> Continue                 = True
    Return maybeE1              ~> Return maybeE2           = maybeE1 ~> maybeE2
    Assignment e11 e12          ~> Assignment e21 e22       = (e11, e12) ~> (e21, e22) 
    _                           ~> _                        = False
    
instance Similar BExpr where
    (Prefixed op1 e1)           ~= (Prefixed op2 e2)        = op1 == op2 && e1 ~= e2
    (Postfixed op1 e1)          ~= (Postfixed op2 e2)       = op1 == op2 && e1 ~= e2
    (Infixed op1 e11 e12)       ~= (Infixed op2 e21 e22)    = op1 == op2 && (e11, e12) ~= (e21, e22)  
    (IdExpr ei1)                ~= (IdExpr ei2)             = ei1 ~= ei2 
    (LitExpr l1)                ~= (LitExpr l2)             = l1 == l2 
    (Call i1 ps1)               ~= (Call i2 ps2)            = (i1, ps1) ~= (i2, ps2) 
    (ArrayAcc i1 idx1)          ~= (ArrayAcc i2 idx2)       = (i1, idx1) ~= (i2, idx2) 
    (Property i1 p1)            ~= (Property i2 p2)         = (i1, p1) ~= (i2, p2)
    (NewArray dt1 e1)           ~= (NewArray dt2 e2)        = dt1 == dt2 && e1 ~= e2
    _                           ~= _                        = False
    
    (Call i1 args1)        ~> (Call i2 args2)      = i1 == i2 
        && length args1 == length args2 
        && and (zipWith (~>) args1 args2)
    (ArrayAcc i1 idx1)     ~> (ArrayAcc i2 idx2)   = (i1, idx1) ~> (i2, idx2)
    (Property i1 p1)       ~> (Property i2 p2)     = (i1, p1) ~> (i2, p2)
    (IdExpr ei1)           ~> (IdExpr ei2)         = ei1 == ei2 
    (LitExpr l1)           ~> (LitExpr l2)         = l1 == l2 
    (NewArray dt1 e1)      ~> (NewArray dt2 e2)    = dt1 == dt2 && e1 ~> e2    
    pre                 ~> post
        | isStrict pre      = False
        | otherwise         = pre == post -- expressions without holes should match exactly
        where
            isStrict Prefixed {}   = False
            isStrict Infixed {}    = False
            isStrict Postfixed {}  = False
            isStrict _             = True 

instance Similar S.Identifier where
    (S.Identifier i1) ~= (S.Identifier i2) = i1 == i2
    
    (S.Identifier i1) ~> (S.Identifier i2) = i1 == i2
        
instance Similar BFragment where
    (BFragment stats1) ~= (BFragment stats2) = stats1 ~= stats2

    (BFragment stats1) ~> (BFragment stats2) = stats1 ~> stats2

instance Similar S.Param where
    (S.Param dt1 id1) ~= (S.Param dt2 id2) = dt1 == dt2 && id1 == id2

    p1 ~> p2 = p1 == p2

instance Similar BClass where
    c1 ~= c2 = error "~= not used for classes"

    (Class idt1 mems1) ~> (Class idt2 mems2) = mems1 ~> mems2

instance Similar BClassMember where
    c1 ~= c2 = error "~= not used for classes"

    c1 ~> c2 = case (c1, c2) of
        (Attribute mdfs1 dt1 exprs1, Attribute mdfs2 dt2 exprs2) -> c1==c2
        (BMethod {}, BMethod {}) -> c1==c2
        (Constructor{}, Constructor{}) -> c1==c2
        _ -> False

instance Similar S.Modifier where
    m1 ~= m2 = m1 == m2
    m1 ~> m2 = m1 == m2