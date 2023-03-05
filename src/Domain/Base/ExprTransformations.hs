------------------------------------------------------------------
-- Transformations for expressions
--
-- Created: 7-11-13, modified x-x-x 
--
------------------------------------------------------------------

module Domain.Base.ExprTransformations
-- 
--    viewAsCNF,
--    viewAsEq,
--    viewAsSum,
--    incExpr

-- )
where

import Domain.Syntax hiding (Statement(..), Expression(..), ForInit(..), AssignOp(..),
    (.||.), (.&&.), (.==.), (.+.), (.<.), (.<=.), (.>.), (.>=.), 
    (.+.), (.*.), (.-.), (./.), (.%.), (-.), (./=.))

import qualified Domain.Syntax as P
import Data.Maybe
import Data.List
import Domain.Evaluator
import Domain.Base.AST
import Utils.Utils
import Ideas.Common.Library hiding ((./.), (.*.), (.%.))

--------------------------------------------------------------------------------
-- Smart constructors for expressions

type BinExprOp = BExpr -> BExpr -> BExpr
type UnExprOp = BExpr -> BExpr 
          
(.+.), (.-.), (.*.), (./.), (.%.),
    (.<.), (.<=.), (.>.), (.>=.), (.==.), (./=.), 
    (.&&.), (.||.) :: BinExprOp

(-.), (!.) :: UnExprOp

l@(LitExpr _) .+. r@(LitExpr _) = fromMaybe (l + r) (calc' Addition l r)
l .+. (Prefixed Minus r) = l - r
(Prefixed Minus l) .+. r = r - l
l .+. r 
    | l == lit0 || l == emptyString = r
    | r == lit0 || r == emptyString = l
    | otherwise = l + r

l@(LitExpr _) .-. r@(LitExpr _) = fromMaybe (l - r) (calc' Subtraction l r)
l .-. (Prefixed Minus r) = l + r
l .-. r 
    | l == lit0 = -r
    | r == lit0 = l
    | isExprHole l && isExprHole r = l - r
    | l == r = lit0
    | otherwise = l - r

l .*. r 
    | isLit l && isLit r    = fromMaybe (l * r) (calc' Multiplication l r)
    | l == lit0 || r == lit0 = lit0
    | l == lit1             = r
    | r == lit1             = l
    | otherwise             = l * r

l ./. r 
    -- causes diag problems
    -- -| isLit l && isLit r    = fromMaybe (Infixed Division l r) (calc' Division l r)
    | l == lit0             = lit0
    | r == lit1             = r
    | otherwise             = Infixed Division l r

l .%. r 
    | isLit l && isLit r    = fromMaybe (Infixed Remainder l r) (calc' Remainder l r)
    | l == lit0             = lit0
    | r == lit1             = lit0
    | otherwise             = Infixed Remainder l r

l .<. r = fromMaybe (Infixed Less l r) (calc' Less l r)

l .<=. r = fromMaybe (Infixed LessOrEqual l r) (calc' LessOrEqual l r)

l .>. r = fromMaybe (Infixed Greater l r) (calc' Greater l r)

l .>=. r = fromMaybe (Infixed GreaterOrEqual l r) (calc' GreaterOrEqual l r)

goe (Infixed GreaterOrEqual l r) = Infixed OR (Infixed Greater l r) (Infixed Equal l r)
goe x = x


l .==. r 
    | isLit l && isLit r  = fromMaybe (Infixed Equal l r) (calc' Equal l r)
    | l == trueLit    = r
    | r == trueLit    = l
    | l == falseLit   = Prefixed Not r
    | r == falseLit   = Prefixed Not l
    | otherwise       = Infixed Equal l r -- Infixed Equal lit0 (r .-. l)

l ./=. r 
    | isLit l && isLit r  = fromMaybe (Infixed NotEqual l r) (calc' NotEqual l r)
    | l == trueLit    = Prefixed Not r
    | r == trueLit    = Prefixed Not l
    | l == falseLit   = r
    | r == falseLit   = l
    | otherwise       = Infixed NotEqual l r 

(-.) (Prefixed Minus e) = e
(-.) (LitExpr (IntLiteral x)) = makeInt (-x)
(-.) e
    | e == lit0 = lit0
    | otherwise = Prefixed Minus e

-- Logic
l .&&. (Prefixed Not r)  | l == r = falseLit
(Prefixed Not l) .&&. r  | l == r = falseLit
l .&&. r
    | l == falseLit || r == falseLit = falseLit
    | l == trueLit                   = r
    | r == trueLit                   = l
    | l == r                         = l
    | otherwise                      = let s = sort [l, r] in Infixed AND (s!!0) (s!!1) --ordering

l .||. (Prefixed Not r)  | l == r = trueLit
(Prefixed Not l) .||. r  | l == r = trueLit
l .||. r
    | l == trueLit || r == trueLit = trueLit
    | l == falseLit                = r
    | r == falseLit                = l 
    | otherwise                    = let s = sort [l, r] in Infixed OR (s!!0) (s!!1) --ordering

(!.) (Prefixed Not e)              = e
(!.) (LitExpr (BoolLiteral n))     = LitExpr $ BoolLiteral $ not n
(!.) (Infixed Equal l r)           = Infixed NotEqual l r
(!.) (Infixed NotEqual l r)        = Infixed Equal l r
(!.) (Infixed Less l r)            = Infixed GreaterOrEqual l r
(!.) e                             = Prefixed Not e

calc' :: InfixOp -> BExpr -> BExpr -> Maybe BExpr     
calc' op (LitExpr ll) (LitExpr rl) = fmap LitExpr$ eitherToMaybe $ calc op ll rl
calc' _ _ _ = Nothing

--------------------------------------------------------------------------------
-- BExpr transformations

simplifyExpr :: BExpr -> BExpr
simplifyExpr expr = case expr of
 
    {- Eliminate unnecessary ops -}
    
    -- only < is used (not possible for floats)
    Infixed LessOrEqual a b    -> (a .<. b) .||. (a .==. b) -- a .<. incExpr 1 b
    Infixed Greater a b        -> b .<. a
    Infixed GreaterOrEqual a b -> (a .>. b) .||. (a .==. b) -- b .<. incExpr 1 a
    -- Eliminate x++ etc
    
    --Postfixed Incr e            -> e .=. (e .+. lit1)
    --Postfixed Decr e            -> e .=. (e .-. lit1)
    
    --Prefixed Incr e -> e .=. (e .+. lit1)
    --Prefixed Decr e -> e .=. (e .-. lit1)
    
    {- Call smart constructors -}
    Infixed op e1 e2 -> 
        let
            smartC = lookup op infixOps
        in maybe expr (\f -> f e1 e2) smartC
    Prefixed Minus e            -> (-.) e
    Prefixed Plus e             -> e
    Prefixed Not e              -> (!.) e
    
    _ -> expr        

    where   
        infixOps = 
            [   (Addition, (.+.)), (Subtraction, (.-.)), (Multiplication, (.*.)),
                (Division, (./.)), (Remainder, (.%.)), (Less, (.<.)),
                (LessOrEqual, (.<=.)), (Greater, (.>.)), (GreaterOrEqual, (.>=.)),
                (Equal, (.==.)), (NotEqual, (./=.)), 
                (AND, (.&&.)), (OR, (.||.))
            ]       
            
-- Views
viewAsSum, viewAsCNF, viewAsEq, viewAsUnEq, viewAsMul :: BExpr -> BExpr        
viewAsSum   = viewAsOp sumView     
viewAsCNF   = simplifyWith (nub . sortExpr) cnfView . simplifyExpr       
viewAsEq    = viewAsOp eqView            
viewAsUnEq  = viewAsOp uneqView      
viewAsMul   = viewAsOp mulView

-- viewAsOp :: View BExpr [BExpr] -> BExpr -> BExpr  
viewAsOp view = simplifyWith sortExpr view . simplifyExpr

sortExpr :: [BExpr] -> [BExpr]
sortExpr xs = lits ++ sort zs
    where
        (lits, zs) = partition isLit2 xs

        -- er is al een isLit
        isLit2:: BExpr -> Bool
        isLit2 (LitExpr _) = True
        isLit2 (Prefixed _ e) = isLit e -- ?
        isLit2 _ = False
        
sumView :: View BExpr [BExpr]
sumView = makeInfixView2 m (.+.) lit0
    where
        m (Infixed Addition x y)    = m x ++ m y
        m (Infixed Subtraction x y) = m x ++ map (-.) (m y)
        
        -- a * (b + c) = (a*b) + (a*c)
        m (Infixed Multiplication a (Infixed Addition b c)) = 
            m (Infixed Multiplication a b) ++ m (Infixed Multiplication a c) 
        m (Infixed Multiplication (Infixed Addition b c) a) = 
            m (Infixed Multiplication a b) ++ m (Infixed Multiplication a c)
        -- a * (b - c) = (a*b) + (a*-c)
        m (Infixed Multiplication a (Infixed Subtraction b c)) = 
            m (Infixed Multiplication a b) ++ m (Infixed Multiplication a ((-.) c))    
        m (Infixed Multiplication (Infixed Subtraction b c) a) = 
            m (Infixed Multiplication a b) ++ m (Infixed Multiplication a ((-.) c)) 
        
        m (Prefixed Minus e)        = map (-.) (m e)
        m expr                      = [expr]
         
cnfView :: View BExpr [BExpr]
cnfView = makeInfixView m (.&&.) trueLit
    where
        -- De Morgan 1
        m (Prefixed Not (Infixed AND p q))  = m ( (!.) p .||. (!.) q)
        -- De Morgan 2: 
        m (Prefixed Not (Infixed OR p q))   = m ((!.) p) ++ m ((!.) q)
        -- Distributivity 1
        m (Infixed OR (Infixed AND p q) r ) = m (p .||. r) ++ m (q .||. r)
        -- Distributivity 2
        m (Infixed OR p (Infixed AND q r))  = m (p .||. q) ++ m (p .||. r)
            
        m (Infixed AND p q)                 = m p ++ m q 
        m ex                                = [ex]

 
eqView :: View BExpr [BExpr]
eqView = makeInfixView2 m (.==.) trueLit
    where
        m (Infixed Equal l r)    = m l ++ m r
        m ex                        = [ex]

uneqView :: View BExpr [BExpr]
uneqView = makeInfixView2 m (./=.) falseLit
    where
        m (Infixed NotEqual l r)    = m l ++ m r
        m ex                        = [ex]

mulView :: View BExpr [BExpr]
mulView = makeInfixView m (.*.) lit1
    where
        m (Infixed Multiplication x y)  = m x ++ m y
        m (Infixed Division x y)        = m x ++ map (1 ./.) (m y) 
        m (Prefixed Minus e)            = map (-.) (m e)
        m expr                          = [expr] 
                
makeInfixView :: (BExpr -> [BExpr]) -> BinExprOp -> BExpr -> View BExpr [BExpr]
makeInfixView m binOp idt = makeView (Just . m) b --(foldl binOp idt)
    where
        --b [] = idt
        b = foldl binOp idt
        
makeInfixView2 :: (BExpr -> [BExpr]) -> BinExprOp -> BExpr -> View BExpr [BExpr]
makeInfixView2 m binOp idt = makeView (Just . m) b --(foldl binOp idt)
    where
        b [] = idt
        b xs = foldl1 binOp xs  
--------------------------------------------------------------------------------
-- Utils
