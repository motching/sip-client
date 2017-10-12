--{-# Language OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module SipClient.Parser where

import Prelude hiding (takeWhile)
import SipClient.Types
import qualified SipClient.Utils as U

import Control.Applicative
import Data.Word8
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as DBC

parseOnly :: A.Parser a -> DBC.ByteString -> Either String a
parseOnly = A.parseOnly

parseSipMessage :: A.Parser SipMessage
parseSipMessage = do
    method <- parseReqMethod
    callId <- parseCallId
    --let !debug = U.trace' $ "parsed method" ++ show method
    return [(ReqMethod, method)
           ,(CallId, callId)]

parseReqMethod :: A.Parser DBC.ByteString
parseReqMethod = A.takeTill isSpace

newline :: A.Parser Word8
newline = A.word8 10

parseCallId :: A.Parser DBC.ByteString
parseCallId = A.string (DBC.pack "Call-ID:") --why does $ not work here
              *> A.takeTill (== 10) --newline - make constant
