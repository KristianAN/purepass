module CLI (runApp) where

import Options.Applicative

data Menu = Menu
  { target :: String
  }

menu :: Parser Menu
menu =
  Menu
    <$> strOption (long "target" <> short 't' <> help "Show password for target")

runMenu :: Menu -> IO ()
runMenu (Menu t) = putStrLn $ "password for " ++ t ++ ": "

runApp :: IO ()
runApp = runMenu =<< execParser opts
  where
    opts =
      info
        (menu <**> helper)
        ( fullDesc
            <> progDesc "Password vault CLI"
            <> header "purepass -- Keep your passwords safe and easily accessible from the CLI"
        )
