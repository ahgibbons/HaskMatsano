{-# LANGUAGE OverloadedStrings #-}
module BlockCipher where


import qualified Data.ByteString as BS
import Data.Word8
import Control.Monad (zipWithM)
import System.Random
import Data.List
import Data.Ord
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

import Control.Monad.Random
import Control.Monad.Random.Class
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error

import CryptoTools
import Types

isBlock :: Int -> BS.ByteString -> Bool
isBlock bs = (==) bs  . BS.length

cipherInitM k = case cipherInit k of
                  CryptoFailed _      -> Nothing
                  CryptoPassed cipher -> Just cipher

-- Encrypt AES 128 bit key size and block size


encAESECBblock :: Key -> PBlock16 -> Maybe CBlock16
encAESECBblock k pblock = do
    maybeBool (isBlock bsAES) pblock
    cipher <- cipherInitM k :: Maybe AES128
    return $ ecbEncrypt cipher pblock

encAESECB :: Key -> PlainText -> Maybe CipherText
encAESECB k ptext = do
  let ptext'   = pkcs7 (fromIntegral bsAES) ptext
  cipher <- cipherInitM k :: Maybe AES128
  return $ ecbEncrypt cipher ptext'


encAESCBCblock :: Key -> CBlock16 -> PBlock16 -> Maybe CBlock16 
encAESCBCblock k cblock pblock = do
    maybeBool (all (isBlock bsAES)) [pblock,cblock] 
    xored <- xorBS pblock cblock
    encAESECBblock k xored

encAESCBC :: Key -> BS.ByteString -> PlainText -> Maybe CipherText
encAESCBC k iv ptext = do
  let ptext'     = pkcs7 (fromIntegral bsAES) ptext
      ptextBlcks = chunksOfBS bsAES ptext'
  ctextBlcks <- scanM (encAESCBCblock k) iv ptextBlcks
  return . BS.concat . tail $ ctextBlcks


decAESECB :: Key -> CipherText -> Maybe PlainText
decAESECB k ctext = do
    maybeBool ((==0) . (`mod` bsAES)) (BS.length ctext)
    cipher <- cipherInitM k :: Maybe AES128
    return $ ecbDecrypt cipher ctext

decAESCBCblock :: Key -> CBlock16 -> CBlock16 -> Maybe PBlock16
decAESCBCblock  k c0 c1 = do
  maybeBool (all (isBlock bsAES)) [c0,c1]
  decrypted <- decAESECB k c1
  xorBS decrypted c0 


decAESCBC :: Key -> BS.ByteString -> CipherText -> Maybe PlainText
decAESCBC k iv ctext = do
  maybeBool (isBlock bsAES) ctext
  let ctextBlcks = chunksOfBS bsAES ctext
  ptextBlcks <- zipWithM (decAESCBCblock k) (iv:ctextBlcks) ctextBlcks    
  return $ BS.concat ptextBlcks

bsAES = 16 :: Int

rawECBDecrypter :: Int -> Int -> (PlainText -> Maybe CipherText) 
                -> Int -> BS.ByteString -> [Word8] -> Maybe [Word8]
rawECBDecrypter msglen bs encryptf n buffer known
  | n == msglen = Just []
  | otherwise     = do
             enc <- encryptf buffer
             let encstring = BS.append buffer (BS.pack known)
             tencs <- mapM (encryptf . BS.snoc encstring) [0..]
             let enc' = BS.take bs
                      . BS.drop ((n `div` bs)*bs) $ enc
                 tencs' = map (BS.take bs . BS.drop ((n `div` bs)*bs)) tencs
                 encsmap = zip tencs' [0..]                 
             w <- lookup enc' encsmap
             let buffer' = if BS.null buffer
                           then (BS.replicate (bs-1) _A)
                           else BS.tail buffer
             (:) w <$> rawECBDecrypter msglen bs encryptf (n+1) buffer' (known++[w])

ecbDecrypter :: (PlainText -> Maybe CipherText) -> Maybe [Word8]
ecbDecrypter encryptf = rawECBDecrypter len bs encryptf 0 (BS.replicate (bs-1) _A) []
    where
      bs  = getECBBS encryptf
      len = getECBMsgLength encryptf


findByte :: Int -> (PlainText -> Maybe CipherText) -> PlainText -> Maybe Word8
findByte bsize encrypter buffer = do
  allencs <- mapM (encrypter . BS.snoc buffer) [0..]
  let allencs' = map (BS.take bsize) allencs
      encsmap  = zip allencs' [0..]
  enc <- BS.take bsize <$> encrypter buffer
  lookup enc encsmap

-- Encryption Oracle

secretEncrypt :: (RandomGen g) => PlainText -> Rand g (Maybe CipherText)
secretEncrypt ptext = do
  ecb <- getRandom
  k   <- randKey bsAES
  iv  <- randIV bsAES
  let ctext = case ecb of 
                True  -> encAESECB k ptext
                False -> encAESCBC k iv ptext
  return ctext

detectionOracle :: CipherText -> CipherMode
detectionOracle ctext = if reps>1 then ECB else CBC
  where
    reps = fst . head . reverse
         . sortBy (comparing fst) 
         . frequency $ chunks
    frequency list = map (\l -> (length l, head l)) (group (sort list))
    chunks = chunksOf bsAES $ BS.unpack ctext

encryptionOracle :: RandomGen g => PlainText -> g -> Maybe CipherMode
encryptionOracle ptext g = do
  ctext <- evalRand encryptOracleR g
  return $ detectionOracle ctext
  where
    encryptOracleR = do
      ebc <- getRandom :: (RandomGen g) => Rand g Bool
      fpaddC <- getRandomR (5,10)
      bpaddC <- getRandomR (5,10)
      let fpadd = BS.replicate fpaddC 0
          bpadd = BS.replicate bpaddC 0
          ptext' = BS.concat [fpadd,ptext,bpadd]
      secretEncrypt ptext'

getECBMsgLength :: (PlainText -> Maybe CipherText) -> Int
getECBMsgLength encrypter = gml 0
  where olen = BS.length . fromJust . encrypter $ ""
        gml n = if (BS.length . fromJust . encrypter . BS.replicate n) _A == olen
                then gml (n+1)
                else (olen - n)

getECBBS :: (PlainText -> Maybe CipherText) -> Int
getECBBS encrypter = gbs 0
  where
    olen = BS.length . fromJust . encrypter $ ""
    gbs n = let len = (BS.length . fromJust . encrypter . BS.replicate n) _A in
            if len == olen
               then gbs (n+1)
               else (len - olen)
