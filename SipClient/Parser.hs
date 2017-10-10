{-# Language OverloadedStrings #-}
module SipClient.Parser where

import Prelude hiding (takeWhile)
import SipClient.Types

import Data.Word (Word8)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as DBC

parseOnly :: A.Parser a -> DBC.ByteString -> Either String a
parseOnly = A.parseOnly

isSpace :: Word8 -> Bool
isSpace c = c /= toEnum(fromEnum ' ') --this is ugly

-- parseSipMessage :: A.Parser SipMessage
-- parseSipMessage = do
--   let method = parseReqMethod

parseReqMethod :: A.Parser DBC.ByteString
parseReqMethod = A.takeWhile isSpace

sipVersionParser :: A.Parser DBC.ByteString
sipVersionParser = A.string "SIP/2.0"
