{-# Language OverloadedStrings #-}
module SipClient.Parser where

import Prelude hiding (takeWhile)
import SipClient.Types

import Data.Word8
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as DBC

parseOnly :: A.Parser a -> DBC.ByteString -> Either String a
parseOnly = A.parseOnly

parseSipMessage :: A.Parser SipMessage
parseSipMessage = do
    method <- parseReqMethod
    --let !debug = U.trace' $ "parsed method" ++ show method
    return [(ReqMethod, method)]

parseReqMethod :: A.Parser DBC.ByteString
parseReqMethod = A.takeTill isSpace

sipVersionParser :: A.Parser DBC.ByteString
sipVersionParser = A.string "SIP/2.0"
