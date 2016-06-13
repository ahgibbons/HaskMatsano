{-# LANGUAGE OverloadedStrings #-}
module CryptTools where

import Prelude
import Numeric
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Read as BSR
import qualified Data.ByteString.Char8 as BSC
import Data.Word8
import Data.List.Split (chunksOf)
import qualified Data.Bits.Bitwise as Bit
import Data.List (sortBy, sort, group, unfoldr, transpose, scanl')
import Data.Ord (comparing)
import Data.Bits
import Data.Maybe
import Data.Tuple (swap)
import Data.Char (intToDigit)
import Data.List.Split (chunksOf)
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Control.Monad (mfilter, foldM, liftM, liftM2, zipWithM)
import Control.Applicative ((<$>),(<*>))
import qualified Data.ByteString.Base64 as B64
import System.Random
import Control.Monad.Random
import Control.Monad.Random.Class (getRandom,getRandoms)

data CipherMode = ECB | CBC deriving (Show)

type HexString = BS.ByteString
type B64String = BS.ByteString
type Key = BS.ByteString
type PlainText = BS.ByteString
type CipherText = BS.ByteString
type PBlock16 = BS.ByteString
type CBlock16 = BS.ByteString

---- Formatting ----

showBin n = showIntAtBase 2 intToDigit n ""

readB16Int :: HexString -> Integer
readB16Int s = read $ "0x" ++ BSC.unpack s

showB16Int :: Integer -> HexString
showB16Int i = BSC.pack $ (showHex i) ""


showB64Int :: Integer -> B64String
showB64Int n = BSC.pack $ map (fromJust . flip lookup b64ListMapC . fromIntegral) b64bits
  where
    b64bits = splitBits 6 n

b64ListC = ['A'..'Z']++['a'..'z']++['0'..'9']++['+','/']
b64ListMapC = zip [0..] b64ListC :: [(Int,Char)]
b64ListMapFlipC = zip b64ListC [0..] :: [(Char,Int)]

--

b64List = [_A.._Z]++[_a.._z]++[_0.._9]++[_plus,_slash]
b64ListMap = zip [0..] b64List :: [(Word8, Word8)]
b64ListMapFlip = zip b64List [0..] :: [(Word8, Word8)]


--readB64 :: B64String -> Maybe BS.ByteString
readB64 b64s
  | BS.length b64s `rem` 4 == 0 = 
      Just $ BS.pack . concat
                     . map parseChunk
                     . chunksOf 4 
                     . map (fromJust . flip lookup b64ListMapFlip)
                     . filter (/= _equal) 
                     .  BS.unpack $ b64s
  | otherwise                   = Nothing
  where
    parseChunk [a,b,c,d] = [shiftL a 2 .|. shiftR b 4
                          , shiftL (b .&. 0x0f) 4 .|. shiftR c 2
                          , shiftL (c .&. 0x03) 6 .|. d]
    parseChunk [a,b,c]   = [shiftL a 2 .|. shiftR b 4
                          , shiftL (b .&. 0x0f) 4 .|. shiftR c 2]
    parseChunk [a,b]     = [shiftL a 2 .|. shiftR b 4]


readB16 :: HexString -> BS.ByteString
readB16 hs
  | BS.length hs `rem` 2 == 0 =
        BS.pack $ map (fromIntegral . readB16Int) $ unfoldr (mSplitAt 2) hs
  | otherwise                 = readB16 $ BS.cons _0 hs
         
showB16 :: BS.ByteString -> HexString
showB16 bs = BS.concat $ map (showB16Int . fromIntegral) $ BS.unpack bs



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

-- Old ByteString - Int conversions


xorChar :: Bits a => [a] -> a -> [a]
xorChar bitlist bit = map (xor bit) bitlist

xorCharBS :: BS.ByteString -> Word8 -> BS.ByteString
xorCharBS bs w = BS.map (xor w) bs

xorBS :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
xorBS a b 
  | BS.length a == BS.length b = Just $ BS.pack $ BS.zipWith xor a b
  | otherwise                  = Nothing

xorB16 :: HexString -> HexString -> HexString
xorB16 h1 h2 = showB16Int $ xor (readB16Int h1) (readB16Int h2)

showB64String :: [Word8] -> B64String
showB64String s
    | r == 0    = showB64Int $ joinBits 8 s
    | otherwise = let t = BS.unpack $ showB64Int $ joinBits 8 (s++ replicate (3-r) 0) 
                  in BS.pack $ take (length t - (3-r)) t ++ (replicate (3-r) _equal)
  where
    r = length s `mod` 3


paddFront :: a -> Int -> [a] -> [a]
paddFront padding bsize list
    | lenMod == 0 = list
    | otherwise   = (replicate (bsize - lenMod) padding) ++ list
      where lenMod = length list `mod` bsize

pkcs7 :: Word8 -> BS.ByteString -> BS.ByteString
pkcs7 blocksize bs = BS.append bs $ BS.pack $ replicate n (fromIntegral n) 
  where n = fromIntegral blocksize - (BS.length bs `mod` fromIntegral blocksize)

paddBack :: Int -> Word8 -> BS.ByteString -> BS.ByteString
paddBack blocksize w bs = BS.append bs $ BS.pack $ replicate n' w
  where n = fromIntegral blocksize - (BS.length bs `mod` fromIntegral blocksize)
        n' = if n==blocksize then 0 else n

letterPerc :: [Double]
letterPerc = map (/120) [8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,0.153,0.772,4.025,2.406,6.749,7.507,1.929,0.095,5.987,6.327,9.056,2.758,0.978,2.361,0.15,1.974,0.074,20]

letterFreqs :: [(Word8,Double)]
letterFreqs = zip ([_a.._z]++[_space]) letterPerc

countChars  :: BS.ByteString -> Map.Map Word8 Int
countChars bs = BS.foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty lowerBs
  where lowerBs = BS.map toLower bs

englishScore :: BS.ByteString -> Double
englishScore bs = sum $ zipWith (*) charFreq letterPerc
  where
    charMap = countChars bs
    charVector = map (\c -> fromIntegral $ fromMaybe 0 (Map.lookup c charMap)) ([_a.._z]++[_space])
    charFreq = map (/ (fromIntegral $ BS.length bs)) charVector


mSplitAt :: Int -> BS.ByteString -> Maybe (BS.ByteString,BS.ByteString)
mSplitAt n "" = Nothing
mSplitAt n bs = Just $ BS.splitAt n bs

decodeHexString :: [Word8] -> HexString -> (Double, BS.ByteString)
decodeHexString chars hs = head . reverse . sortBy (comparing fst) $ scores
  where
    allDecodes  = map (xorCharBS . readB16 $ hs) chars
    scores = zip (map englishScore allDecodes) allDecodes
--stringToHex :: BS.ByteString -> HexString

decodeBSXOR :: [Word8] -> BS.ByteString -> (Double, BS.ByteString, Word8)
decodeBSXOR chars bs = head . reverse . sortBy (comparing fst') $ scores
  where
    fst' (a,_,_) = a
    allDecodes = map (xorCharBS bs) chars
    scores  = zip3 (map englishScore allDecodes) allDecodes chars

repeatXOR :: BS.ByteString -> BS.ByteString -> BS.ByteString
repeatXOR key string = BS.pack $ zipWith xor (cycle $ BS.unpack key) (BS.unpack string)

hammingDist :: BS.ByteString -> BS.ByteString -> Maybe Int
hammingDist a b
  | BS.length a == BS.length b = Just $ sum . map popCount $ BS.zipWith xor a b
  | otherwise                  = Nothing

decodeRepeatXOR keyLen text = map (decodeBSXOR [0.._z] . BS.pack) chunksT
  where
    chunks  = chunksOf keyLen $ BS.unpack text
    chunksT = transpose chunks
    

keyLenScores :: [Int] -> BS.ByteString -> [(Int, Double)]
keyLenScores lenrange text = sortBy (comparing snd)
    . zip lenrange . map (\n -> keyScore n text) $ lenrange

keyScore :: Int -> BS.ByteString -> Double
keyScore n bs = average . map (\i -> fromIntegral i / fromIntegral n)
              . catMaybes $ zipWith hammingDist blocks (tail blocks)
  where
    blocks = map BS.pack $ chunksOf n $ BS.unpack bs

average xs = (/) (sum xs) (fromIntegral . length $ xs)

takeEvery :: Int -> [a] -> [a]
takeEvery n xs = case drop (n-1) xs of
                   (y:ys) -> y : takeEvery n ys
                   []     -> []



-- Block Ciphers

cipherInitM k = case cipherInit k of
                  CryptoFailed _      -> Nothing
                  CryptoPassed cipher -> Just cipher

maybeBool :: (a -> Bool) -> a -> Maybe a
maybeBool f a = if (f a) then Just a else Nothing

isBlock :: BS.ByteString -> Bool
isBlock = (==) bsAES . BS.length

-- Encrypt AES 128 bit key size and block size


encAESECBblock :: Key -> PBlock16 -> Maybe CBlock16
encAESECBblock k pblock = do
    maybeBool isBlock pblock
    cipher <- cipherInitM k :: Maybe AES128
    return $ ecbEncrypt cipher pblock

encAESECB :: Key -> PlainText -> Maybe CipherText
encAESECB k ptext = do
  let ptext'   = pkcs7 (fromIntegral bsAES) ptext
  cipher <- cipherInitM k :: Maybe AES128
  return $ ecbEncrypt cipher ptext'

encAESECB' :: Key -> PlainText -> Maybe CipherText
encAESECB' k ptext = do
  let ptext'     = pkcs7 (fromIntegral bsAES) ptext
      ptextBlcks = map BS.pack $ chunksOf bsAES $ BS.unpack ptext'
  ctextBlcks <- mapM (encAESECBblock k) ptextBlcks
  return $ BS.concat ctextBlcks

encAESCBCblock :: Key -> CBlock16 -> PBlock16 -> Maybe CBlock16 
encAESCBCblock k cblock pblock = do
    maybeBool (all isBlock) [pblock,cblock] 
    xored <- xorBS pblock cblock
    encAESECBblock k xored

encAESCBC :: Key -> BS.ByteString -> PlainText -> Maybe CipherText
encAESCBC k iv ptext = do
  let ptext'     = pkcs7 (fromIntegral bsAES) ptext
      ptextBlcks = map BS.pack $ chunksOf bsAES $ BS.unpack ptext'
  ctextBlcks <- scanM (encAESCBCblock k) iv ptextBlcks
  return . BS.concat . tail $ ctextBlcks


decAESECB :: Key -> CipherText -> Maybe PlainText
decAESECB k ctext = do
    maybeBool ((==0) . (`mod` bsAES)) (BS.length ctext)
    cipher <- cipherInitM k :: Maybe AES128
    return $ ecbDecrypt cipher ctext

decAESCBCblock :: Key -> CBlock16 -> CBlock16 -> Maybe PBlock16
decAESCBCblock  k c0 c1 = do
  maybeBool (all isBlock) [c0,c1]
  decrypted <- decAESECB k c1
  xorBS decrypted c0 


decAESCBC :: Key -> BS.ByteString -> CipherText -> Maybe PlainText
decAESCBC k iv ctext = do
  maybeBool isBlock ctext
  let ctextBlcks = map BS.pack $ chunksOf bsAES $ BS.unpack ctext
  ptextBlcks <- zipWithM (decAESCBCblock k) (iv:ctextBlcks) ctextBlcks    
  return $ BS.concat ptextBlcks

bsAES = 16 :: Int

scanM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanM f y0 []     = return [y0]
scanM f y0 (x:xs) = do
      y  <- f y0 x
      ys <- scanM f y xs
      return $ y0:ys

-- Encryption Oracle

secretEncrypt :: (RandomGen g) => PlainText -> Rand g (Maybe CipherText)
secretEncrypt ptext = do
  ecb <- getRandom
  k   <- randKey
  iv  <- randIV
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
      let fpadd = BS.pack $ replicate fpaddC 0
          bpadd = BS.pack $ replicate bpaddC 0
          ptext' = BS.concat [fpadd,ptext,bpadd]
      secretEncrypt ptext'

randKey :: RandomGen g => Rand g Key
randKey = do
  kstream <- getRandoms
  return . BS.pack . take bsAES $ kstream

randIV :: RandomGen g => Rand g CBlock16
randIV = do
  stream <- getRandoms
  return . BS.pack . take bsAES $ stream





ecbDecrypter :: Int -> B64String -> Maybe [Word8]
ecbDecrypter blocksize bstring = findByte . BS.pack . replicate 15 $ _A
  where
    ustring = fromJust . readB64 $ bstring
    k       = getKey $ mkStdGen 0
    findByte tstring = do
      let allstrs = zip (map (BS.snoc tstring) [0..]) [0..]
      enc <- BS.take blocksize <$> encAESECB k tstring
      lookup enc allstrs



findByte k bsize ustring tstring = do
  allencs <- mapM (encAESECB k . BS.snoc tstring) [0..]
  let allencs' = map (BS.take bsize) allencs
      allstrs = zip allencs' [0..]
  enc <- BS.take bsize <$> encAESECB k (BS.append tstring ustring)
  lookup enc allstrs

-- KeyGen

getKey :: RandomGen r => r -> Key
getKey g = BS.pack . take bsAES . randoms $ g

-- Text values

ttext = "Hello World, My name is Andrew!!" :: PlainText
tkey = "YELLOW SUBMARINE" :: Key
tblock = "Hello World! Bye" :: BS.ByteString

tiv = BS.pack $ replicate (fromIntegral bsAES) 0

longtext = "The debate has not changed, and it gets to the core foundation of modern health care. The SBM position is quite straightforward as a profession, health care providers owe it to the public to base their advice and interventions on the best available science and evidence. It is our duty to establish and enforce a standard of care that includes adequate due diligence in determining the safety and effectiveness of interventions. The standard of care also includes giving patients proper informed consent and ethical standards of professionalism. There is also a well-established standard for conducting research on humans." :: BS.ByteString




