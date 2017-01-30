module Types where

import qualified Data.ByteString as BS

data CipherMode = ECB | CBC deriving (Show)

type DynRec a b = [(a,b)]
type KV = BS.ByteString

type HexString = BS.ByteString
type B64String = BS.ByteString
type Key = BS.ByteString
type PlainText = BS.ByteString
type CipherText = BS.ByteString
type PBlock16 = BS.ByteString
type CBlock16 = BS.ByteString
