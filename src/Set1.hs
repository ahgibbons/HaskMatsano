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

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error

-- Challenge 1 inText
-- Hex to B64
inText :: HexString
inText = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
chall1 = showB64String $ BS.unpack inText
chall1_res :: B64String
chall1_res = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"


-- Challenge 2 xor two strings
ch2_t1 :: HexString
ch2_t1 = "1c0111001f010100061a024b53535009181c"

ch2_t2 :: HexString
ch2_t2 = "686974207468652062756c6c277320657965"

chall2 :: HexString
chall2 = showB16Int $ (readB16Int ch2_t1) `xor` (readB16Int ch2_t2)

chall2_res :: HexString
chall2_res = "746865206b696420646f6e277420706c6179"


-- Challenge 3
-- Single byte XOR cipher
ch3_instring :: HexString
ch3_instring = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"


chall3 = decodeBSXOR [_0.._z] $ readB16 ch3_instring

-- Challenge 4
filePath = "4.txt"

chall4 = do
  fileText <- BS.readFile filePath
  let inlines = BSC.lines fileText
      scores = map (decodeBSXOR [_0.._z] . readB16) inlines
  print $ head . reverse . sortBy (comparing fst') $ scores
    where
      fst' (a,_,_) = a

-- Challenge 5
ch5_inText = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" :: BS.ByteString
ch5_key = "ICE" :: BS.ByteString

chall5 = showB16 $ repeatXOR ch5_key ch5_inText


-- Challenge 6
ch6_keyRange = [2..40]
ch6_t1 = "this is a test" :: BS.ByteString
ch6_t2 = "wokka wokka!!!" :: BS.ByteString

inFile = BS.readFile "6.txt"
inTextIO = do
  inText <- inFile
  return $ fromJust . readB64 . BS.filter (/= _lf) $ inText

chall6 = do
  inText <- inTextIO
  let k = BS.pack $ map (\(_,_,a) -> a) $ decodeRepeatXOR 29 inText
      t = repeatXOR k inText
  return t


-- Challenge 7
ch7_key = "YELLOW SUBMARINE" :: BS.ByteString

ch7_inText = BS.readFile "7.txt"

chall7 = do
  fileText <- ch7_inText
  let ctext = fromJust . readB64 . BS.filter (/= _lf) $ fileText
      CryptoPassed aes128 = cipherInit ch7_key :: CryptoFailable AES128
      ptext = ecbDecrypt aes128 ctext
  print ptext

-- Challenge 8
ch8_inText = BS.readFile "8.txt"


{-}
chall8 = do
  fileText <- ch8_inText
  let tlines = BSC.lines fileText
      chunkLines = map (chunksOf 32 . BS.unpack) tlines
      chunkLinesBS = map BS.pack tlines
-}
