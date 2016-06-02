{-# LANGUAGE OverloadedStrings #-}
module Set1 where

import CryptTools
import qualified Data.ByteString as BS
import Data.Bits
import Data.Char

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
chall2 = showB16 $ (readB16 ch2_t1) `xor` (readB16 ch2_t2)

chall2_res :: HexString
chall2_res = "746865206b696420646f6e277420706c6179"


-- Challenge 3
-- Single byte XOR cipher
ch3_instring :: HexString
ch3_instring = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"


chall3 = map (xorCharBS (hexToString ch3_instring) . fromIntegral . ord) ['A'..'z']
