module Crypto (encryptData, decryptData) where

import Codec.Binary.Base64 qualified as Base64
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, KeySizeSpecifier (..), makeIV, nullIV)
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA256)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Maybe (fromJust)

saltLength :: Int
saltLength = 8

ivLength :: Int
ivLength = 16

keyLength :: Int
keyLength = 32

iterationCount :: Int
iterationCount = 65536

encryptData :: ByteString -> ByteString -> IO ByteString
encryptData str secret = do
  salt <- getRandomBytes saltLength
  iv <- getRandomBytes ivLength
  let iv' = fromJust (makeIV iv :: Maybe (IV AES256))
  let key = convert (fastPBKDF2_SHA256 (Parameters iterationCount keyLength) secret salt :: ByteString) :: ByteString
  case cipherInit key of
    CryptoFailed err -> error (show err)
    CryptoPassed cipher -> do
      let e = ctrCombine cipher iv' str
      pure $ B.concat [salt, iv, e]

decryptData :: ByteString -> ByteString -> ByteString
decryptData encryptedData secret =
  let (salt, rest) = B.splitAt saltLength encryptedData
      (iv, ciphertext) = B.splitAt ivLength rest
      iv' = fromJust (makeIV iv :: Maybe (IV AES256))
      key = convert (fastPBKDF2_SHA256 (Parameters iterationCount keyLength) secret salt :: ByteString) :: ByteString
   in case cipherInit key of
        CryptoFailed err -> error (show err)
        CryptoPassed cipher -> ctrCombine cipher iv' ciphertext
