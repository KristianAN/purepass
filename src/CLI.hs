module CLI (runApp) where

import Database.SQLite.Simple
import Options.Applicative

data PwOperations = PwOperations
  { target :: String,
    password :: String,
    insert :: Bool
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
            <*> switch (long "insert" <> short 'i' <> help "insert new password for target")
        )

menu :: Parser Menu
menu = pwOps <|> listAllOps

runMenu :: Connection -> Menu -> IO ()
runMenu conn menu = case menu of
  PwOps pwOps -> pure ()
  ListAllOps all -> pure ()

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
