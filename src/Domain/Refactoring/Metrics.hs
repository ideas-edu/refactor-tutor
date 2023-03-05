module Domain.Refactoring.Metrics where

import Domain.Syntax
import Domain.Evaluator
import Domain.Base.AST (BClassMember)
import Domain.Base.Conversion (conv)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe
import Ideas.Common.Constraint
import Ideas.Common.Library hiding (Alt)
import Domain.Printer

c1 :: Constraint (Context ClassMember)
c1 = makeConstraint "CC" (ccConstraint . fromContext)
    where
        ccConstraint p = case p of
            Nothing -> Irrelevant
            Just p' | cyclomaticComplexity p' > 5 -> Error "Cyclomatic Complexity too high"
                    | otherwise                   -> Ok ()

c2 :: TestCase -> Constraint (Context ClassMember)
c2 tc = makeConstraint "test" (testConstraint . fromContext)
    where
        testConstraint :: Maybe ClassMember -> Result ()
        testConstraint p = case p of
            Nothing -> Irrelevant
            Just p' -> case testMethodWith1 (conv p' :: BClassMember) tc of
                        Left e      -> Error (show e)
                        Right True  -> Ok ()
                        Right False -> Error ("Test case failed, calling " ++ mName ++ " with "
                            ++ show (showTC tc) ++ ", but your method returns " ++ maybe "nothing" showPretty lit ++ "." ++ testcaseMsg)
                where
                    Right lit   = evalMethod (conv p' :: BClassMember) (testIn tc)
                    mName       = fromMaybe "?" (methodName p')
                    testcaseMsg = fromMaybe "" (testMsg tc)

logLinesOfCode :: IsProgram p => p -> Int
logLinesOfCode = sum . map loc . toP
  where
    loc stat = case stat of
        Block _ xs          -> sum $ map loc xs 
        If _ s              -> loc s + 1
        IfElse _ s1 s2      -> loc s1 + loc s2 + 2
        While _ s           -> loc s + 1
        For _ _ _ s         -> loc s + 1
        ForEach _ _ _ s     -> loc s + 1
        Feedback _ s        -> loc s
        MustUse s           -> loc s
        Alt xs              -> maximum $ map loc xs 
        Empty               -> 0
        ExprStat _          -> 1
        VarDeclarations _ _ -> 1
        ArrayDecls _ _      -> 1
        Break               -> 1
        Continue            -> 1
        Print _             -> 1
        _                   -> error (show stat)

-- https://www.leepoint.net/principles_and_practices/complexity/complexity-java-method.html
-- TODO not complete
cyclomaticComplexity :: IsProgram p => p -> Int
cyclomaticComplexity pr = let p = toP pr in
    1 + sum 
    (
       [ 1 | If {} <- universeBi p]
    ++ [ 1 | IfElse {} <- universeBi p]
    ++ [ 1 | For {} <- universeBi p]
    ++ [ 1 | ForEach {} <- universeBi p]
    ++ [ 1 | While {} <- universeBi p]
    ++ [ 1 | Break <- universeBi p]
    ++ [ 1 | Continue <- universeBi p]
    ++ [ 1 | Infixed AND _ _ <- universeBi p]
    ++ [ 1 | Infixed OR _ _ <- universeBi p]
    )

-- Calculation from: https://docs.sonarqube.org/display/SONAR/Metrics+-+Complexity (Java)
cyclomaticComplexity2 :: ClassMember -> Int
cyclomaticComplexity2 m@Method {} = let p = mBody m in
    1 + sum 
    (
       [ 1 | If {} <- universeBi p]
    ++ [ 1 | IfElse {} <- universeBi p]
    ++ [ 1 | For {} <- universeBi p]
    ++ [ 1 | ForEach {} <- universeBi p]
    ++ [ 1 | While {} <- universeBi p]
    ++ [ 1 | Break <- universeBi p]
    ++ [ 1 | Continue <- universeBi p]
    ++ [ 1 | Infixed AND _ _ <- universeBi p]
    ++ [ 1 | Infixed OR _ _ <- universeBi p]
    )
    + min 0 ( sum [ 1 | Return _ <- universeBi p] - 1) -- count all returns expect last, if any. 
        -- TO DO if a return else return
    
cyclomaticComplexity2 _ = undefined

-- https://www.sonarsource.com/docs/CognitiveComplexity.pdf
cognitiveComplexity :: Program -> Int
cognitiveComplexity = undefined