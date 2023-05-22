{-# LANGUAGE OverloadedStrings #-}
module RegressionTests where

import Database.HDBC.Sqlite3
import Database.HDBC
import System.IO
import System.Environment
import System.Exit
import System.Process
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.List

-- | Order of diagnose results, from worst to best (higher index is better)
status_list :: [Text]
status_list = ["", "syntaxerror", "notequiv", "similar", "correct", "buggy", "expected"]

-- | Get score from status string
getScore :: Text -> Int
getScore status = fromMaybe 0 $ elemIndex status status_list

-- | Run query (`sql`) on sqlite database (`dbPath`)
loadFromDB :: String -> String -> IO [[SqlValue]]
loadFromDB dbPath sql = do
    conn <- connectSqlite3 dbPath
    dat <- quickQuery' conn sql []
    disconnect conn
    return dat

-- | Get list of input requests from sqlite database with path `database`
getInputs :: String -> IO [String]
getInputs database = do
  results <- loadFromDB database "select input from requests where service = 'diagnoser';"
  return $ map (fromSql . head) results 

-- | Run compiled rpt at path `file` with input request `input`
runRpt :: String -> String -> IO String
runRpt file input = do
  _ <- writeFile "input.json" input
  readProcess file ["--file=input.json"] ""

-- | From result json object, retrieve diagnose result (one of `status_list`)
getStatus :: String -> Text
getStatus result = fromMaybe "" $ do
  obj <- (decode $ BLU.fromString result :: Maybe Object)
  status <- (parseMaybe (\o -> o .: "result") obj :: Maybe Object)
  return $ fst $ head $ HM.toList status
  
-- | Process a list of requests by comparing `base` and `compare` (two paths to rpt executables)
-- `diff` and `count` start off being `0`
-- Returns a score (number of steps improved in `status_list` overall) and number of better diagnoses
processEntry :: String -> String -> Int -> Int -> [String] -> IO (Int, Int)
processEntry base compare dif count [] = return (dif, count)
processEntry base compare dif count (input:xs) = do

  old_output <- runRpt base input
  new_output <- runRpt compare input

  let new_status = getStatus new_output
  let old_status = getStatus old_output

  let new_points = getScore new_status
  let old_points = getScore old_status

  let difference = new_points - old_points
  
  if difference >= 0
    then processEntry base compare (dif + difference) (if difference == 0 then count else count + 1) xs
    else do
      putStrLn "Test case failed!"
      print old_status
      print old_points

      print new_status
      print new_points
      
      putStrLn input
      putStrLn old_output
      putStrLn new_output
      
      error "Test case failed!"  

-- Handle input arguments
main :: IO ()
main = do
  args <- getArgs
  case args of
    [database, base, compare] -> do
      items <- getInputs database
      (score, count) <- processEntry base compare 0 0 items
      putStrLn $ "New score: " ++ show score
      putStrLn $ "New diagnoses matched: " ++ show count
      return ()
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <database> <base> <compare>"
      hPutStrLn stderr $ "  database: path to sqlite3 database with data to compare against"
      hPutStrLn stderr $ "  base:     path to cgi executable of the base system"
      hPutStrLn stderr $ "  compare:  path to cgi executable of the system to compare against"
      exitFailure
