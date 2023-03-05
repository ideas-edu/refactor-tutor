module Test.Domain.Normalisation where

import Utils.FileUtils
import Utils.Utils
import Domain.Parsers.JavaParser
import Domain.Syntax
import Domain.Base.AST
import Domain.Printer
import Domain.Refactoring.Rules.LoopRules
import Domain.Refactoring.Equality (cluster)
import Domain.Transformation.ProgramTransformation
import Domain.Refactoring.HCExercises
import Domain.Evaluator

import Data.Generics.Uniplate.DataOnly (transformBi)
import Data.Time
import Data.List
import Data.Function
import Data.Maybe

import Control.Arrow
import Text.PrettyPrint.Leijen (Pretty)
import System.Directory
import System.FilePath

svTemplate = CodeTemplate { 
    pre  = "public class Test { public static int sumValues(int [] values, boolean positivesOnly) {",
    post = "}}" 
}

go = do
    programs <- loadPrograms

    t1 <- getCurrentTime
    putStrLn "Clustering"

    let cs = map (second cluster) programs
        gp = groupClusters cs
        showProgram (n, p) = "* " ++ n ++ "\n"
        showCluster xs = unlines (map fst xs) ++ "\n--->\n" ++ (show . pretty . snd . head $ xs) ++ "\n"
        showClusters cs = intercalate "---\n" $ map showCluster cs
    putStrLn $ showClusters gp
    putStrLn $ "Nr of clusters " ++ show (length gp)
    getCurrentTime >>= printElapsedTime t1

    -- check correctness
    let clusters = map (snd . head) gp
    let c = map (maybe True (flip testMethodWithSucceeds testCasesSumValues) . findMethod ) clusters
    putStrLn "All tests succeeded:"
    print c 

go1 = do
    programs <- loadPrograms
    putStrLn "Choose idx"
    i <- getLine 
    let c = second cluster $ programs!!(read i)
    return c

loadPrograms :: IO [(String, Program)]
loadPrograms = do 
    curDir <- getCurrentDirectory
    jp <- loadNamedProgramsFromDir (parseWithTemplate svTemplate) javaLang (curDir </> "..//test//p1intprograms")
    putStrLn $ "Loaded " ++ show (length jp) ++ " java programs"  
    putStrLn $ unlines $ map fst jp
    return jp

groupClusters :: (Eq b, Ord b) => [(a, b)] -> [[(a, b)]]
groupClusters = groupBy ((==) `on` snd) . sortBy (compare `on` snd)

groupNames :: (Eq b, Ord b) => [(a, b)] -> [[a]]
groupNames = map (map fst) . groupClusters

groupPrograms :: (Eq b, Ord b) => [(a, b)] -> [[b]]
groupPrograms = map (map snd) . groupClusters

printClusters :: Pretty a => [[a]] -> String
printClusters = unlines . map (intercalate "," . map (show . pretty))
