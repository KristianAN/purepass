{-# LANGUAGE OverloadedStrings #-}

module VaultSpec where

import Control.Exception (SomeException, try)
import Data.Either (isLeft, isRight)
import Database.SQLite.Simple
import Test.Hspec
import Vault (PasswordVault (..))

setup :: IO Connection
setup = do
  conn <- open ":memory:"
  _ <- execute_ conn "create table passwords (id integer primary key not null, target text not null, password not null)" :: IO ()
  pure conn

spec :: Spec
spec = before setup $
  describe "Tests for vault using sqlite" $ do
    it "can insert a password" $ \conn -> do
      res <- insert conn "amuchlongerkeythatwecanuse" "testOne" "testOnePw"
      res `shouldBe` Just "testOnePw"

    it "can get inserted pw" $ \conn -> do
      _ <- insert conn "key" "testTwo" "testTwoPw"
      res <- get conn "key" "testTwo"
      res `shouldBe` Just "testTwoPw"

    it "can delete inserted pw" $ \conn -> do
      _ <- insert conn "key" "testThree" "testThreePw"
      _ <- delete conn "testThree"
      res <- get conn "key" "testThree"
      res `shouldBe` Nothing

    it "can update inserted pw" $ \conn -> do
      _ <- insert conn "key" "testFour" "testFourPw"
      _ <- update conn "key" "testFour" "TestFourPwUpdated"
      res <- get conn "key" "testFour"
      res `shouldBe` Just "TestFourPwUpdated"

    it "fetching password with incorrect key fails" $ \conn -> do
      _ <- insert conn "key" "testFive" "testFivePw"
      res <- get conn "wrongkey" "testFive" -- :: IO (Either SomeException (Maybe String))
      res `shouldNotBe` Just "testFivePw"
