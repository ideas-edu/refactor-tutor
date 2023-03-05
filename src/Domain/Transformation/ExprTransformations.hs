module Domain.Transformation.ExprTransformations where

import Domain.Syntax
import Data.Maybe
import Data.List
import Domain.Evaluator
import Utils.Utils
import Ideas.Common.Library (View, makeView, simplifyWith, IsView(..), match)

--------------------------------------------------------------------------------
-- Smart constructors for expressions

type BinExprOp = Expression -> Expression -> Expression
type UnExprOp  = Expression -> Expression 


(..+..), (..-..), (..*..), (../..), (..%..),
    (..<..), (..<=..), (..>..), (..>=..), (..==..), (../=..), 
    (..=..),
    (..&&..), (..||..) :: BinExprOp

(-..), (!..) :: UnExprOp

--l ..+.. r = undefined
l@(LiteralExpr _) ..+.. r@(LiteralExpr _) = fromMaybe (l + r) (calc' Addition l r)
l ..+.. (Prefixed Minus r) = l - r
(Prefixed Minus l) ..+.. r = r - l
l ..+.. r 
    | l == lit0 || l == emptyString = r
    | r == lit0 || r == emptyString = l
    | otherwise = l + r

l@(LiteralExpr _) ..-.. r@(LiteralExpr _) = fromMaybe (l - r) (calc' Subtraction l r)
l ..-.. (Prefixed Minus r) = l + r
l ..-.. r 
    | l == lit0 = -r
    | r == lit0 = l
    | isExprHole l && isExprHole r = l - r
    | l == r = lit0
    | otherwise = l - r

l ..*.. r 
    | isLit l && isLit r    = fromMaybe (l * r) (calc' Multiplication l r)
    | l == lit0 || r == lit0 = lit0
    | l == lit1             = r
    | r == lit1             = l
    | otherwise             = l * r

l ../.. r 
    -- causes diag problems
    -- -| isLit l && isLit r    = fromMaybe (Infixed Division l r) (calc' Division l r)
    | l == lit0             = lit0
    | r == lit1             = r
    | otherwise             = l ./. r

l ..%.. r 
    | isLit l && isLit r    = fromMaybe (l .%. r) (calc' Remainder l r)
    | l == lit0             = lit0
    | r == lit1             = lit0
    | otherwise             = l .%. r

l ..<.. r  = fromMaybe (l .<. r)  (calc' Less l r)

l ..<=.. r = fromMaybe (l .<=. r) (calc' LessOrEqual l r)

l ..>.. r  = fromMaybe (l .>. r)  (calc' Greater l r)

l ..>=.. r = fromMaybe (l .>=. r) (calc' GreaterOrEqual l r)

l ..==.. r 
    | isLit l && isLit r  = fromMaybe (l .==. r) (calc' Equal l r)
    | l == trueLit    = r
    | r == trueLit    = l
    | l == falseLit   = nt r
    | r == falseLit   = nt l
    | otherwise       = l .==. r -- Infixed Equal lit0 (r ..-.. l)

l ../=.. r 
    | isLit l && isLit r  = fromMaybe (l .!=. r ) (calc' NotEqual l r)
    | l == trueLit    = nt r
    | r == trueLit    = nt l
    | l == falseLit   = r
    | r == falseLit   = l
    | otherwise       = l .!=. r 

l ..=.. r = Assignment Assign l r

(-..) (Prefixed Minus e) = e
(-..) (LiteralExpr (IntLiteral x)) = makeInt (-x)
(-..) e
    | e == lit0 = lit0
    | otherwise = Prefixed Minus e

-- Logic
l ..&&.. r
    | l == falseLit || r == falseLit = falseLit
    | l == trueLit = r
    | r == trueLit = l
    | l == r = l
    | otherwise = Infixed AND l r

l ..||.. r
    | l == trueLit || r == trueLit = trueLit
    | l == falseLit = r
    | r == falseLit = l
    | otherwise = Infixed OR l r

(!..) (Prefixed Not e)              = e
(!..) (LiteralExpr (BoolLiteral n))     = LiteralExpr $ BoolLiteral $ not n
(!..) (Infixed Equal l r)           = Infixed NotEqual l r
(!..) (Infixed NotEqual l r)        = Infixed Equal l r
(!..) (Infixed Less l r)            = Infixed GreaterOrEqual l r
(!..) e                             = Prefixed Not e

calc' :: InfixOp -> Expression -> Expression -> Maybe Expression     
calc' op (LiteralExpr ll) (LiteralExpr rl) = fmap LiteralExpr$ eitherToMaybe $ calc op ll rl
calc' _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Expression transformations

simplifyExpr :: Expression -> Expression
simplifyExpr expr = case expr of
 
    {- Eliminate unnecessary ops -}
    
    -- only < is used (not possible for floats)
    Infixed LessOrEqual a b    -> a ..<.. incExpr 1 b
    Infixed Greater a b        -> b ..<.. a
    Infixed GreaterOrEqual a b -> b ..<.. incExpr 1 a
    -- Eliminate x++ etc
    Postfixed Incr e            -> e ..=.. (e ..+.. lit1)
    Postfixed Decr e            -> e ..=.. (e ..-.. lit1)
    
    --Prefixed Incr e -> e ..=.. (e ..+.. lit1)
    --Prefixed Decr e -> e ..=.. (e ..-.. lit1)
    
    -- Eliminate += etc
    Assignment AssignAdd a b    -> a ..=.. (a ..+.. b)
    Assignment AssignSub a b    -> a ..=.. (a ..-.. b)
    Assignment AssignMul a b    -> a ..=.. (a ..*.. b)
    Assignment AssignDiv a b    -> a ..=.. (a ../.. b)
    Assignment AssignRem a b    -> a ..=.. (a ..%.. b)
    
    {- Call smart constructors -}
    Infixed op e1 e2 -> 
        let
            smartC = lookup op infixOps
        in maybe expr (\f -> f e1 e2) smartC
    Prefixed Minus e            -> (-..) e
    Prefixed Plus e             -> e
    Prefixed Not e              -> (!..) e
    
    _ -> expr        

    where   
        infixOps = 
            [   (Addition, (..+..)), (Subtraction, (..-..)), (Multiplication, (..*..)),
                (Division, (../..)), (Remainder, (..%..)), (Less, (..<..)),
                (LessOrEqual, (..<=..)), (Greater, (..>..)), (GreaterOrEqual, (..>=..)),
                (Equal, (..==..)), (NotEqual, (../=..)), 
                (AND, (..&&..)), (OR, (..||..))
            ]       
            
-- Views
viewAsSum, viewAsCNF, viewAsEq, viewAsUnEq, viewAsMul :: Expression -> Expression        
viewAsSum   = viewAsOp sumView     
viewAsCNF   = simplifyWith (nub . sortExpr) cnfView . simplifyExpr       
viewAsEq    = viewAsOp eqView            
viewAsUnEq  = viewAsOp uneqView      
viewAsMul   = viewAsOp mulView

-- viewAsOp :: View Expression [Expression] -> Expression -> Expression  
viewAsOp view = simplifyWith sortExpr view . simplifyExpr

sortExpr :: [Expression] -> [Expression]
sortExpr xs = lits ++ sort zs
    where
        (lits, zs) = partition isLit2 xs

        -- er is al een isLit
        isLit2:: Expression -> Bool
        isLit2 (LiteralExpr _) = True
        isLit2 (Prefixed _ e) = isLit e -- ?
        isLit2 _ = False
        
sumView :: View Expression [Expression]
sumView = makeInfixView2 m (..+..) lit0
    where
        m (Infixed Addition x y)    = m x ++ m y
        m (Infixed Subtraction x y) = m x ++ map (-..) (m y)
        
        -- a * (b + c) = (a*b) + (a*c)
        m (Infixed Multiplication a (Infixed Addition b c)) = 
            m (Infixed Multiplication a b) ++ m (Infixed Multiplication a c) 
        m (Infixed Multiplication (Infixed Addition b c) a) = 
            m (Infixed Multiplication a b) ++ m (Infixed Multiplication a c)
        -- a * (b - c) = (a*b) + (a*-c)
        m (Infixed Multiplication a (Infixed Subtraction b c)) = 
            m (Infixed Multiplication a b) ++ m (Infixed Multiplication a ((-..) c))    
        m (Infixed Multiplication (Infixed Subtraction b c) a) = 
            m (Infixed Multiplication a b) ++ m (Infixed Multiplication a ((-..) c)) 
        
        m (Prefixed Minus e)        = map (-..) (m e)
        m expr                      = [expr]
         
cnfView :: View Expression [Expression]
cnfView = makeInfixView m (..&&..) trueLit
    where
        -- De Morgan 1
        m (Prefixed Not (Infixed AND p q))  = m ( (!..) p ..||.. (!..) q)
        -- De Morgan 2: 
        m (Prefixed Not (Infixed OR p q))   = m ((!..) p) ++ m ((!..) q)
        -- Distributivity 1
        m (Infixed OR (Infixed AND p q) r ) = m (p ..||.. r) ++ m (q ..||.. r)
        -- Distributivity 2
        m (Infixed OR p (Infixed AND q r))  = m (p ..||.. q) ++ m (p ..||.. r)
            
        m (Infixed AND p q)                 = m p ++ m q 
        m ex                                = [ex]

 
eqView :: View Expression [Expression]
eqView = makeInfixView2 m (..==..) trueLit
    where
        m (Infixed Equal l r)    = m l ++ m r
        m ex                        = [ex]

uneqView :: View Expression [Expression]
uneqView = makeInfixView2 m (../=..) falseLit
    where
        m (Infixed NotEqual l r)    = m l ++ m r
        m ex                        = [ex]

mulView :: View Expression [Expression]
mulView = makeInfixView m (..*..) lit1
    where
        m (Infixed Multiplication x y)  = m x ++ m y
        m (Infixed Division x y)        = m x ++ map (1 ../..) (m y) 
        m (Prefixed Minus e)            = map (-..) (m e)
        m expr                          = [expr] 
                
makeInfixView :: (Expression -> [Expression]) -> BinExprOp -> Expression -> View Expression [Expression]
makeInfixView m binOp idt = makeView (Just . m) b --(foldl binOp idt)
    where
        --b [] = idt
        b xs = foldl binOp idt xs
        
makeInfixView2 :: (Expression -> [Expression]) -> BinExprOp -> Expression -> View Expression [Expression]
makeInfixView2 m binOp idt = makeView (Just . m) b --(foldl binOp idt)
    where
        b [] = idt
        b xs = foldl1 binOp xs  



--------------------------------------------------------------------------------
-- Utils

