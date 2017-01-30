{-# LANGUAGE OverloadedStrings #-}
module Set1 where

import CryptoTools
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import Data.Char
import Data.List
import Data.Ord (comparing)
import System.IO (readFile)
import Data.Word8
import Data.Maybe (fromJust)
import qualified Data.ByteString.Base64 as B64
import System.Random
import qualified Data.ByteString.Search as SearchBS

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import qualified Text.Email.Validate as Email
import qualified Data.ByteString.Search as SearchBS
import Text.ParserCombinators.Parsec

import Types
import BlockCipher

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

ch12_key = getKey 16 $ mkStdGen 1

newEncrypter :: PlainText -> Key -> PlainText -> Maybe CipherText
newEncrypter bstring key fstring = encAESECB key (BS.append fstring bstring)
  
ch12Encrypter = newEncrypter (fromJust . readB64 $ ch12_intext) ch12_key


ch12_intext = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK" :: B64String

-- Challenge 13
ch13_test = "foo=bar&baz=qux&zap=zazzle" :: KV
ch13_key = getKey 16 $ mkStdGen 13

ch13Enc = profileForEnc ch13_key
ch13Dec = decProfile ch13_key

profileFor :: BS.ByteString -> Maybe KV
profileFor email = do
    maybeBool Email.isValid email
    let email' = safeEmail email
    return $ makeKVString email' 10 "user"


profileForEnc :: Key -> BS.ByteString -> Maybe CipherText
profileForEnc key email = kvData >>= encAESECB key
  where kvData = profileFor email 


decProfile :: Key -> CipherText -> Either ParseError [(String,String)]
decProfile key ctext = kvParser ptext
  where ptext = fromJust $ decAESECB key ctext >>= unpkcs7 


safeEmail :: BS.ByteString -> BS.ByteString
safeEmail = BL.toStrict
          . SearchBS.replace "=" ("\\=" :: BS.ByteString)
          . BL.toStrict
          . SearchBS.replace "&" ("\\&" :: BS.ByteString)


makeKVString :: BS.ByteString -> Int -> BS.ByteString -> BS.ByteString
makeKVString email uid role = let uid' = BSC.pack (show uid) in
                              BS.concat ["email=",email,
                                         "&uid=",uid',"&role=",role]

kvParser :: KV -> Either ParseError [(String,String)]
kvParser input = map toTuple <$> parse kvString "Error Text?" (BSC.unpack input)
  where
    toTuple [x,y] = (x,y)    
kvString = sepBy kvVal (char '&')
kvVal = sepBy (many kvChar) (char '=')
kvChar = noneOf "\\&=" <|> (char '\\' >> anyChar)
