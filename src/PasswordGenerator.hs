{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PasswordGenerator (generateForRandom, PwFlags (..), RandomPw (..)) where

import Control.Monad (replicateM)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Functor
import Data.Text
import GHC.Generics
import Paths_purepass (getDataFileName)
import System.Random

data WordPw = WordPw
  { wLen :: Int,
    number :: Bool
  }

data RandomPw = RandomPw
  { rLen :: Int,
    special :: Bool
  }

newtype WordList = WordList {words :: [String]} deriving (Generic, Show)

instance FromJSON WordList

getWordFile :: IO (Maybe WordList)
getWordFile = do
  path <- getDataFileName "resources/words.json"
  content <- BL.readFile path
  pure (decode content :: Maybe WordList)

data PwFlags
  = WordPwFlag WordPw
  | RandomPwFlag RandomPw

generateForRandom :: PwFlags -> IO String
generateForRandom (RandomPwFlag flags) =
  let chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ if special flags then "!@#$%^&*()" else ""
   in replicateM (rLen flags) (randomChar chars)
generateForRandom _ = undefined

randomChar :: String -> IO Char
randomChar input = do
  r <-
    let len = Prelude.length input - 1
     in randomRIO (0, len)
  pure $ input !! r
