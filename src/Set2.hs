{-# LANGUAGE OverloadedStrings #-}
module Set1 where

import CryptTools
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Bits
import Data.Char
import Data.List
import Data.Ord (comparing)
import System.IO (readFile)
import Data.Word8
import Data.Maybe (fromJust)
import qualified Data.ByteString.Base64 as B64
import System.Random

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error

-- Challenge 9 -- Padding
ch9_text = "YELLOW SUBMARINE" :: BS.ByteString
chall9 = pkcs7 20 ch9_text

-- Challenge 10 -- AES CBC
ch10_inFile = BS.readFile "10.txt"
ch10_key = "YELLOW SUBMARINE" :: Key
ch10_iv = BS.pack $ replicate 16 0
chall10 = do
  inText <- ch10_inFile
  let text = fromJust . readB64 . BS.filter (/= _lf) $ inText
  return $ decAESCBC ch10_key ch10_iv text

-- Challenge 11


-- Challenge 12

ch12_key = getKey $ mkStdGen 1

newEncrypter :: B64String -> Key -> PlainText -> Maybe CipherText
newEncrypter b64string key fstring = do
  bstring <- readB64 b64string
  encAESECB key (BS.append fstring bstring)
  
ch12Encrypter = newEncrypter ch12_intext ch12_key


ch12_intext = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK" :: B64String

