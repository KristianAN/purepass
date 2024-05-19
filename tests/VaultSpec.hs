{-# LANGUAGE OverloadedStrings #-}

module VaultSpec where

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
      res <- insert conn "testOne" "testOnePw"
      res `shouldBe` Just "testOnePw"

    it "can get inserted pw" $ \conn -> do
      _ <- insert conn "testTwo" "testTwoPw"
      res <- get conn "testTwo"
      res `shouldBe` Just "testTwoPw"

    it "can delete inserted pw" $ \conn -> do
      _ <- insert conn "testThree" "testThreePw"
      _ <- delete conn "testThree"
      res <- get conn "testThree"
      res `shouldBe` Nothing

    it "can update inserted pw" $ \conn -> do
      _ <- insert conn "testFour" "testFourPw"
      _ <- update conn "testFour" "TestFourPwUpdated"
      res <- get conn "testFour"
      res `shouldBe` Just "TestFourPwUpdated"
