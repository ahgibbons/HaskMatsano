{-# LANGUAGE OverloadedStrings #-}
module CryptTools where

import Prelude
import Numeric
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Read as BSR
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Word as W
import qualified Data.Word8 as W8
import Data.List.Split (chunksOf)
import qualified Data.Bits.Bitwise as Bit
import Data.List (sortBy, sort, group, unfoldr)
import Data.Ord (comparing)
import Data.Bits
import Data.Maybe
import Data.Char (intToDigit)
import Data.Tuple (swap)
import Data.Char

type HexString = BS.ByteString
type B64String = BS.ByteString


-- Show number in Binary form
showBin n = showIntAtBase 2 intToDigit n ""

-- Join list of numbers into one number in bit chunks of byteshift
joinBits :: (Integral a, Num b, Bits b) => Int -> [a] -> b
joinBits byteshift wlist = foldr (.|.) 0 shifted
  where
    bitpos = [length wlist - 1,length wlist -2..0]
    shifted = zipWith (\p n -> shift (fromIntegral n) (byteshift*p)) bitpos wlist

-- Split Number into bit chunks of size bs
splitBits :: (Num a, Bits a) => Int -> a -> [a]
splitBits bs number = reverse $ splitBits' bs number
  where
    splitBits' _ 0        = []
    splitBits' bytesize n = 
        fromIntegral (2^bytesize-1) .&. n : splitBits' bytesize (shiftR n bytesize)


b64List = ['A'..'Z']++['a'..'z']++['0'..'9']++['+','/']
b64ListMap = zip [0..] b64List :: [(Int,Char)]
b64ListMapFlip = zip b64List [0..] :: [(Char,Int)]


readB16 :: HexString -> Integer
readB16 s = read $ "0x" ++ BSC.unpack s

showB16 :: Integer -> HexString
showB16 i = BSC.pack $ (showHex i) ""


showB64 :: Integer -> B64String
showB64 n = BSC.pack $ map (fromJust . flip lookup b64ListMap . fromIntegral) b64bits
  where
    b64bits = splitBits 6 n

xorChar :: Bits a => [a] -> a -> [a]
xorChar bitlist bit = map (xor bit) bitlist

xorCharBS :: BS.ByteString -> W.Word8 -> BS.ByteString
xorCharBS bs w = BS.map (xor w) bs

xorB16 :: HexString -> HexString -> HexString
xorB16 h1 h2 = showB16 $ xor (readB16 h1) (readB16 h2)

showB64String :: [W.Word8] -> B64String
showB64String s
    | r == 0    = showB64 $ joinBits 8 s
    | otherwise = let t = BS.unpack $ showB64 $ joinBits 8 (s++ replicate (3-r) 0) 
                  in BS.pack $ take (length t - (3-r)) t ++ (replicate (3-r) W8._equal)
  where
    r = length s `mod` 3


{-}
readB64 :: B64String -> Integer
readB64 s = 
    where
      byte3chunks = chunksOf 3 $ padd 0 3 $ BS.unpack s
      byte3val    = 
-}

padd :: a -> Int -> [a] -> [a]
padd padding bsize list
    | lenMod == 0 = list
    | otherwise   = (replicate (bsize - lenMod) padding) ++ list
      where lenMod = length list `mod` bsize



letterPerc :: [Double]
letterPerc = map (/100) [8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,0.153,0.772,4.025,2.406,6.749,7.507,1.929,0.095,5.987,6.327,9.056,2.758,0.978,2.361,0.15,1.974,0.074]

letterFreqs :: [(Char,Double)]
letterFreqs = zip ['a'..'z'] letterPerc

countChars :: String -> String -> [(Char, Int)]
countChars alphabet string = map (\cs -> (head cs, length cs))
                           . group . sort . filter (`elem` alphabet) $ string

charCountVector :: String -> [(Char, Int)] -> [Int]
charCountVector alphabet charCount = map (flip countLookup charCount) alphabet
  where
    countLookup c cs = case lookup c cs of
                         Nothing -> 0
                         Just n  -> n

similarity :: Integral a => [Double] -> [a] -> Double
similarity charFreq testVect = dot charFreq (map (/charNum) testVect')
  where
    dot v1 v2 = sum $ zipWith (*) v1 v2
    testVect' = map fromIntegral testVect
    charNum = sum testVect'

hexToString :: HexString -> BS.ByteString
hexToString hs
  | BS.length hs `rem` 2 == 0 =
        BS.pack $ map (fromIntegral . readB16) $ unfoldr (mSplitAt 2) hs
  | otherwise                 = hexToString $ BS.cons W8._0 hs
         
stringToHex :: BS.ByteString -> HexString
stringToHex bs = BS.concat $ map (showB16 . fromIntegral) $ BS.unpack bs

mSplitAt :: Int -> BS.ByteString -> Maybe (BS.ByteString,BS.ByteString)
mSplitAt n "" = Nothing
mSplitAt n bs = Just $ BS.splitAt n bs

--stringToHex :: BS.ByteString -> HexString
