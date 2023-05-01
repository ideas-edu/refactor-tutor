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

status_list :: [Text]
status_list = ["", "syntaxerror", "notequiv", "similar", "correct", "expected", "buggy"]

getScore :: Maybe Text -> Int
getScore status = fromMaybe 0 $ elemIndex (fromMaybe "" status) status_list

loadFromDB :: String -> String -> IO [[SqlValue]]
loadFromDB dbPath sql = do
    conn <- connectSqlite3 dbPath
    dat <- quickQuery' conn sql []
    disconnect conn
    return dat

getInputOutput :: String -> IO [(String, String)]
getInputOutput database = do
  results <- loadFromDB database "select input, output from requests where service = 'diagnoser';"
  return $ map (\(x:y:_) -> (fromSql x, fromSql y)) results 

runRpt :: String -> String -> IO String
runRpt file input = do
  _ <- writeFile "input.json" input
  readProcess file ["--file=input.json"] ""

getStatus :: String -> Maybe Text
getStatus result = do
  obj <- (decode $ BLU.fromString result :: Maybe Object)
  status <- (parseMaybe (\o -> o .: "result") obj :: Maybe Object)
  return $ fst $ head $ HM.toList status

processEntry :: String -> String -> Int -> [(String, String)] -> IO Int
processEntry base compare dif [] = return dif
processEntry base compare dif ((input, _):xs) = do

  old_output <- runRpt base input
  new_output <- runRpt compare input

  let new_status = getStatus new_output
  let old_status = getStatus old_output

  let new_points = getScore new_status
  let old_points = getScore old_status

  let difference = new_points - old_points
  
  if difference >= 0
    then processEntry base compare (dif + difference) xs
    else do
      putStrLn "Test case failed!"
      print old_status
      print old_points

      print new_status
      print new_points
      
      putStrLn input
      putStrLn old_output
      putStrLn new_output
      
      return 0  

main :: IO ()
main = do
  args <- getArgs
  case args of
    [database, base, compare] -> do
      items <- getInputOutput database
      score <- processEntry base compare 0 items
      putStrLn $ "New score: " ++ show score
      return ()
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <database> <base> <compare>"
      hPutStrLn stderr $ "  database: path to sqlite3 database with data to compare against"
      hPutStrLn stderr $ "  base:     path to cgi executable of the base system"
      hPutStrLn stderr $ "  compare:  path to cgi executable of the system to compare against"
      exitFailure
