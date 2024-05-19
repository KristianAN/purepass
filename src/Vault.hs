{-# LANGUAGE OverloadedStrings #-}

module Vault (PasswordVault (..), VaultState) where

import Control.Concurrent (modifyMVar, readMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor
import Data.Map.Strict as Map
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.MVar (MVar (MVar), readMVar)
import PasswordGenerator (PwFlags (RandomPwFlag), RandomPw (..), generateForRandom)

-- Encrypt a string using botan bindings
encrypt :: String -> IO String
encrypt = pure

-- Decrypt a string using botan bindings
decrypt :: String -> IO String
decrypt = pure

data DeleteRes = Success | Failure

class PasswordVault m s where
  get :: s -> String -> m (Maybe String)
  insert :: s -> String -> String -> m (Maybe String)
  update :: s -> String -> String -> m (Maybe String)
  delete :: s -> String -> m ()
  listAllTargets :: s -> m [String]

-- Sqlite typeclass instance
instance PasswordVault IO Connection where
  get = selectPw
  insert = insertPw
  update = updatePw
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
    [Only str] -> traverse decrypt $ Just str
    _ -> pure Nothing

updatePw :: Connection -> String -> String -> IO (Maybe String)
updatePw conn target newPw = do
  rows <- execute conn "update passwords set password = ? where target = ?" (newPw, target)
  selectPw conn target

deletePw :: Connection -> String -> IO ()
deletePw conn target =
  execute conn "delete from passwords where target = ?" (Only target)

-- An implementation of the typeclass for an MVar to see
-- how to manage mutable state inside the application

newtype VaultState = VaultState (MVar (Map String String))

instance PasswordVault IO VaultState where
  get (VaultState mvar) key = do
    map <- readMVar mvar
    traverse decrypt (Map.lookup key map)

  insert (VaultState mvar) target pw =
    modifyMVar mvar $ \state ->
      let new = Map.insert target pw state
       in pure (new, Map.lookup target new)

  update (VaultState mvar) target newPass = modifyMVar mvar $ \state ->
    let new = Map.insert target newPass state
     in pure (new, Map.lookup target new)

  delete (VaultState mvar) target = modifyMVar mvar $ \state ->
    pure (Map.delete target state, ())

  listAllTargets (VaultState mvar) = fmap Map.keys (readMVar mvar)
