{-# LANGUAGE OverloadedStrings #-}

module CLI (runApp) where

import Data.Foldable qualified
import Data.Functor
import Data.List (intercalate)
import Database.SQLite.Simple
import Options.Applicative
import Vault qualified as PWV

data PwOperations = PwOperations
  { target :: String,
    password :: String,
    insert :: Maybe String
  }

data Menu
  = PwOps PwOperations
  | ListAllOps Bool

listAllOps :: Parser Menu
listAllOps = ListAllOps <$> switch (long "list" <> short 'l' <> help "List all targets")

pwOps :: Parser Menu
pwOps =
  PwOps
    <$> ( PwOperations
            <$> strOption (long "target" <> short 't' <> help "Show password for target")
            <*> strOption (long "password" <> short 'p' <> help "Your superpassword")
            <*> optional (strOption (long "insert" <> short 'i' <> help "insert new password for target"))
        )

menu :: Parser Menu
menu = pwOps <|> listAllOps

runMenu :: Connection -> Menu -> IO ()
runMenu conn menu = case menu of
  PwOps pwOps ->
    case insert pwOps of
      Just pw -> do
        _ <- PWV.insert conn (password pwOps) (target pwOps) pw
        let out = "Inserted password for target " ++ target pwOps
         in putStrLn out
      Nothing -> do
        pw <- PWV.get conn (password pwOps) (target pwOps)
        Data.Foldable.for_ pw putStrLn
  ListAllOps all -> PWV.listAllTargets conn >>= putStrLn . intercalate "\n"

runApp :: Connection -> IO ()
runApp conn = runMenu conn =<< execParser opts
  where
    opts =
      info
        (menu <**> helper)
        ( fullDesc
            <> progDesc "Password vault CLI"
            <> header "purepass -- Keep your passwords safe and easily accessible from the CLI"
        )
