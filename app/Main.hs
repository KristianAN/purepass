module Main where

import CLI (runApp)
import Database.SQLite.Simple

main :: IO ()
main = do
  conn <- open "db.sql"
  runApp conn
