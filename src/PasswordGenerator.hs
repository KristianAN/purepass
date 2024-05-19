module PasswordGenerator (generateForRandom, PwFlags (..), RandomPw (..)) where

import Control.Monad (replicateM)
import Data.Functor
import System.Random

data WordPw = WordPw
  { wLen :: Int,
    number :: Bool
  }

data RandomPw = RandomPw
  { rLen :: Int,
    special :: Bool
  }

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
    let len = length input - 1
     in randomRIO (0, len)
  pure $ input !! r
