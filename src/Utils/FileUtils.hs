module Utils.FileUtils where

import Domain.Syntax
import Domain.Parsers.JavaParser

import System.Directory
import System.FilePath
import Data.List
import Control.Monad
import Control.Exception
import Data.Either

data Language = Language 
    { 
        extension   :: String
    } deriving Show

javaLang, phpLang :: Language
javaLang = Language "java"
phpLang  = Language "php"

type PParser = String -> Either String Program


-- | Dir should exist
loadProgramsFromDir :: PParser -> Language -> FilePath -> IO [Program]
loadProgramsFromDir parser lang = fmap (map snd) . loadNamedProgramsFromDir parser lang

loadNamedProgramsFromDir :: PParser -> Language -> FilePath -> IO [(FilePath, Program)]
loadNamedProgramsFromDir parser lang dir = 
    listDirectory dir 
      >>= mapM parseFile . filterExt 
      >>= return . rights -- parse and only return parsed files 
    where     
        parseFile fp = fmap (fmap (\p -> (fp,p)) . parser) . readFile . combine dir $ fp
        filterExt = filter (isSuffixOf $ extension lang)


-- | Locate a known code file extension in a folder
findLangtype :: FilePath -> IO (Maybe Language)
findLangtype = fmap find . listDirectory
    where
        find files
            | findExtension javaLang files = Just javaLang
            | findExtension phpLang files  = Just phpLang
            | otherwise                    = Nothing
        findExtension  = any . isSuffixOf . extension 


-- | Reads file or returns Nothing
-- TODO does not support unicode characters
readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists fp = fmap Just (readFile fp) `catch`
    \e -> const (return Nothing) (e :: IOException)

forceReadMethod :: FilePath -> IO Program
forceReadMethod = fmap forceParseMethod . readFile

readMethod :: FilePath -> IO (Maybe Program)
readMethod fp = (parseMethod' =<<) <$> readFileIfExists fp 

readClassMember :: FilePath -> IO (Maybe ClassMember)
readClassMember fp = (stripMethod =<<) <$> readMethod fp