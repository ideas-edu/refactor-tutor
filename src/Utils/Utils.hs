module Utils.Utils
(
    listFilesR, listSubFolders,
    eitherToMaybe,
    notNull,
    printElapsedTime,
    loopTransformations, fixpoint, loopTrans
    , toTuple2, toTuple3, toTuple4, three2two
    , asMaybe, fromMaybeId
    , toTerm1, toTerm2, toTerm3, toTerm4, toTerm5
    , fromTerm1, fromTerm2, fromTerm3, fromTerm4, fromTerm5
    , fromTermError, toTermError
    , trim
)
where

import Data.List (isSuffixOf, dropWhileEnd, dropWhile)
import Data.Time
import Data.Char (isSpace)
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Semigroup
import Control.Monad 
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (joinPath)
import Ideas.Common.Library


trim = dropWhileEnd isSpace . dropWhile isSpace

toTuple2 :: a -> b -> (a, b)
toTuple2 a b = (a, b)

toTuple3 :: a -> b -> c -> (a, b, c)
toTuple3 a b c = (a, b, c)

toTuple4 :: a -> b -> c -> d -> (a, b, c, d)
toTuple4 a b c d = (a, b, c, d)

three2two :: (a, b, c) -> (a, b)
three2two (x, y, _) = (x, y)

-- Returns nothing if unchanged 
asMaybe :: Eq a => (a -> a) -> a -> Maybe a
asMaybe f x = let res = f x in if res == x then Nothing else Just res

fromMaybeId :: (a -> Maybe a) -> a -> a
fromMaybeId f x = fromMaybe x (f x)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

isDODD :: String -> Bool
isDODD f = not $ (isSuffixOf "." f) || (isSuffixOf ".." f)

listDirs :: [FilePath] -> IO [FilePath]
listDirs = filterM doesDirectoryExist

listFiles :: [FilePath] -> IO [FilePath]
listFiles = filterM doesFileExist

joinFN :: String -> String -> FilePath
joinFN p1 p2 = joinPath [p1, p2]
    
-- | List all files (recursively) in a directory
-- Source: http://therning.org/magnus/archives/228            
listFilesR :: FilePath -> IO [FilePath]
listFilesR path = 
    do
        allfiles <- getDirectoryContents path
        no_dots <- filterM (return . isDODD) (map (joinFN path) allfiles)
        dirs <- listDirs no_dots
        subdirfiles <- (mapM listFilesR dirs >>= return . concat)
        files <- listFiles no_dots
        return $ files ++ subdirfiles
        
-- | List all folders in a directory          
listSubFolders :: FilePath -> IO [FilePath]
listSubFolders path = 
   do
        allfiles <- getDirectoryContents path
        no_dots <- filterM (return . isDODD) (map (joinFN path) allfiles)
        listDirs no_dots

notNull :: [a] -> Bool
notNull = not . null 

printElapsedTime :: UTCTime -> UTCTime -> IO ()
printElapsedTime t1 t2 = putStrLn $ "[Elapsed time " ++ show (diffUTCTime t2 t1) ++ "]"

-------------------------------------------------------------------------------
-- Transformation

loopTransformations :: (Eq a) => (a -> a) -> a -> a
loopTransformations transformations = doTrans 1
    where
        doTrans count p  
            | count > 50 = error "normalisation loop"
            | otherwise =
                let transformed = transformations p
                in if transformed ==  p 
                        then p
                        else doTrans (count + 1) transformed
                        
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = stop . iterate f
 where
   stop []           = error "Ideas.Common.Utils: empty list"
   stop (x:xs)
      | x == head xs = x
      | otherwise    = stop xs                         

loopTrans :: (Eq a) => (a -> a) -> a -> (a, Int)
loopTrans transformations = doTrans 0
    where
        doTrans count p  
            | count > 50 = error "transformation loop"
            | otherwise =
                let transformed = transformations p
                in if transformed == p 
                        then (p, count)
                        else doTrans (count + 1) transformed

-------------------------------------------------------------------------------
-- Terms

toTerm1 :: (IsTerm a) => a -> [Term]
toTerm2 :: (IsTerm a, IsTerm b) => a -> b -> [Term]
toTerm3 :: (IsTerm a, IsTerm b, IsTerm c) => a -> b -> c -> [Term]
toTerm4 :: (IsTerm a, IsTerm b, IsTerm c, IsTerm d) => a -> b -> c -> d -> [Term]
toTerm5 :: (IsTerm a, IsTerm b, IsTerm c, IsTerm d, IsTerm e) => a -> b -> c -> d -> e -> [Term]
toTerm1 a        = [toTerm a]
toTerm2 a b      = [toTerm a, toTerm b]
toTerm3 a b c    = [toTerm a, toTerm b, toTerm c]
toTerm4 a b c d  = [toTerm a, toTerm b, toTerm c, toTerm d]
toTerm5 a b c d e = [toTerm a, toTerm b, toTerm c, toTerm d, toTerm e]

fromTerm1 :: (MonadPlus m, IsTerm a) => (a -> b) -> Term -> m b
fromTerm2 :: (MonadPlus m, IsTerm a, IsTerm b)  => (a -> b -> c) -> Term -> Term -> m c
fromTerm3 :: (MonadPlus m, IsTerm a, IsTerm b, IsTerm c)  => (a -> b -> c -> d) -> Term -> Term -> Term -> m d
fromTerm4 :: (MonadPlus m, IsTerm a, IsTerm b, IsTerm c, IsTerm d)  => (a -> b -> c -> d -> e) -> Term -> Term -> Term -> Term -> m e
fromTerm5 :: (MonadPlus m, IsTerm a, IsTerm b, IsTerm c, IsTerm d, IsTerm e)  => (a -> b -> c -> d -> e -> f) -> Term -> Term -> Term -> Term -> Term -> m f
fromTerm1 con = liftM con . fromTerm
fromTerm2 con a b = liftM2 con (fromTerm a) (fromTerm b)
fromTerm3 con a b c = liftM3 con (fromTerm a) (fromTerm b) (fromTerm c)
fromTerm4 con a b c d = liftM4 con (fromTerm a) (fromTerm b) (fromTerm c) (fromTerm d)
fromTerm5 con a b c d e = liftM5 con (fromTerm a) (fromTerm b) (fromTerm c) (fromTerm d) (fromTerm e)

fromTermError :: MonadPlus m => Term -> m a
fromTermError term = fail $ "No fromTerm definition for: " ++ show term

toTermError :: (Show s) => s -> a
toTermError t = error $ "No term for " ++ show t