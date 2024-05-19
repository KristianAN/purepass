module Main where

-- import CLI (runApp)
import Data.List (intercalate)
import Database.SQLite.Simple
-- import PasswordGenerator (PwFlags (..), RandomPw (..), generateForRandom)
import Vault (PasswordVault (..))

main :: IO ()
main = do
  conn <- open "db.sql"
  allTargets <- listAllTargets conn
  -- pw <- generateForRandom $ RandomPwFlag $ RandomPw 10 True
  putStrLn $ intercalate "\n" allTargets
