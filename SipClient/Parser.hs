{-# Language OverloadedStrings #-}


module SipClient.Parser where

import Prelude hiding (takeWhile)
import SipClient.Types

import Data.Word (Word8)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as DB
--import qualified Data.ByteString.Char8 as DBC

parseOnly :: A.Parser a -> DB.ByteString -> Either String a
parseOnly = A.parseOnly

isSpace :: Word8 -> Bool
isSpace c = c /= toEnum(fromEnum ' ') --this is ugly

parseReqMethod :: A.Parser DB.ByteString
parseReqMethod = A.takeWhile isSpace

sipVersionParser :: A.Parser DB.ByteString
sipVersionParser = A.string "SIP/2.0"
