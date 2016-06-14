module CryptoTools.Types where

data CipherMode = ECB | CBC deriving (Show)

type HexString = BS.ByteString
type B64String = BS.ByteString
type Key = BS.ByteString
type PlainText = BS.ByteString
type CipherText = BS.ByteString
type PBlock16 = BS.ByteString
type CBlock16 = BS.ByteString
