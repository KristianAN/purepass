{-# LANGUAGE OverloadedStrings #-}

module Vault (PasswordVault (..)) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto (decryptData, encryptData)
import Data.ByteString.Char8
import Data.Functor
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import PasswordGenerator (PwFlags (RandomPwFlag), RandomPw (..), generateForRandom)

encrypt :: (MonadIO m) => String -> String -> m String
encrypt key input = do
  byte <- liftIO $ encryptData (pack input) (pack key)
  pure $ unpack byte

decrypt :: (MonadIO m) => String -> String -> m String
decrypt key input =
  pure $ unpack (decryptData (pack input) (pack key))

class PasswordVault m s where
  get :: s -> String -> String -> m (Maybe String)
  insert :: s -> String -> String -> String -> m (Maybe String)
  update :: s -> String -> String -> String -> m (Maybe String)
  delete :: s -> String -> m ()
  listAllTargets :: s -> m [String]

-- Sqlite typeclass instance
instance PasswordVault IO Connection where
  get conn key target = do
    pw <- selectPw conn target
    traverse (decrypt key) pw
  insert conn key target pw = do
    encryptedPw <- encrypt key pw
    inserted <- insertPw conn target encryptedPw
    traverse (decrypt key) inserted
  update conn key target pw = do
    encryptedPw <- encrypt key pw
    updatePw conn target encryptedPw
  delete = deletePw
  listAllTargets conn = selectAllTargets conn <&> Prelude.map fromOnly

-- db Schema
-- create table passwords(
--   id integer primary key not null,
--   target text not null,
--   password text not null
-- )

-- Sqlite functions
selectAllTargets :: Connection -> IO [Only String]
selectAllTargets conn =
  query_ conn "select target from passwords" :: IO [Only String]

insertPw :: Connection -> String -> String -> IO (Maybe String)
insertPw conn target pw = do
  _ <- execute conn "insert into passwords (target, password) values (?, ?)" (target, pw)
  selectPw conn target

selectPw :: Connection -> String -> IO (Maybe String)
selectPw conn target = do
  res <- query conn "select password from passwords where target = ?" (Only target) :: IO [Only String]
  case res of
    [Only str] -> pure $ Just str
    _ -> pure Nothing

updatePw :: Connection -> String -> String -> IO (Maybe String)
updatePw conn target newPw = do
  rows <- execute conn "update passwords set password = ? where target = ?" (newPw, target)
  selectPw conn target

deletePw :: Connection -> String -> IO ()
deletePw conn target =
  execute conn "delete from passwords where target = ?" (Only target)
