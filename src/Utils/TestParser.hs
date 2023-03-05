module Utils.TestParser 
 (parseTest)
where

import Domain.Parsers.JavaParser 
import Domain.Syntax
import Domain.Evaluator 
import Utils.Utils
import Data.List.Split
import Control.Monad
import Control.Applicative
import qualified Data.Sequence as S

-- | Parses test cases defined as {1,2,3};5;"x" -> 42 -> fail description
parseTest :: String -> Maybe TestCase
parseTest s = do
    let parts = splitOn "->" s
        part1 = parts!!0
        part2 = parts!!1
        part3 = parts!!2
    guard (length parts >= 2)
    inp  <- if notEmpty part1 then parseParams part1 else Just []
    outp <- parseLit part2
    let desc = if length parts >= 3 then Just part3 else Nothing
    return $ TestCase inp outp desc

-- | {1,2,3};true;"s"
parseParams :: String -> Maybe [Literal]
parseParams = sequence . map parseLit . splitOn ";"

paramToLit :: Expression -> Maybe Literal
paramToLit (LiteralExpr l) = Just l
paramToLit (Prefixed Minus (LiteralExpr (IntLiteral i))) = Just (IntLiteral (-i))
paramToLit (Prefixed Minus (LiteralExpr (DoubleLiteral d))) = Just (DoubleLiteral (-d))
paramToLit _               = Nothing

parseLit :: String -> Maybe Literal
parseLit s = parseArray (trim s) <|> (parseExpr s >>= paramToLit)

-- | Parses {1,2,3}
parseArray :: String -> Maybe Literal
parseArray s = do
    guard (length s >= 2 && head s == '{' && last s == '}')
    let elems = map toLit $ filter notEmpty $ splitOn "," (init $ tail s)
        toLit e = parseExpr e >>= paramToLit
    elems' <- sequence elems
    return $ ArrayLit $ S.fromList elems'

notEmpty :: String -> Bool
notEmpty = not . null . trim

