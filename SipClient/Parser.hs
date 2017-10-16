{-# LANGUAGE BangPatterns #-}

module SipClient.Parser where

import Prelude hiding (takeWhile)

import SipClient.Types

import Data.Word8 hiding (isSpace)
import Debug.Trace
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Char8 as DBC

parseOnly :: AB.Parser a -> DBC.ByteString -> Either String a
parseOnly = AB.parseOnly

isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 13 || w == 10   -- CR or LF

isSpace :: Word8 -> Bool
isSpace w = w == 32 || w == 9        --space or horizontal tab

isColon :: Word8 -> Bool
isColon w = w == 58                  -- colon

parseUntil :: (Word8 -> Bool) -> AB.Parser DBC.ByteString
parseUntil cond = AB.takeTill cond <* AB.skipWhile cond

parseSipMessage :: AB.Parser SipMessage
parseSipMessage = do

    -- trace ("\n" ++ "boopp!") return ()

    !method <-parseUntil isSpace
    !scheme <- parseUntil isColon
    !uri <- parseUntil isSpace
    !version <- parseUntil isEndOfLine

    trace ("\nreqMethod: " ++ show method) return ()
    trace ("\nuriScheme: " ++ show scheme) return ()
    trace ("\nreqUri: " ++ show uri) return ()
    trace ("\nsipVersion: " ++ show version) return ()

    -- --trace ("\ncallId: " ++ show callId) return ()
    -- trace "\nall succeed!" return ()

    return Request { reqMethod = method
                   , uriScheme = scheme
                   , reqUri = uri
                   , sipVersion = version
                   , headers = [(CallId, DBC.pack "call-id")]
                   , body = DBC.pack "body"
                    }


    -- let headers = [(CallId, DBC.pack "call-id")]
    -- let body = DBC.pack "body"

    -- return Request method scheme uri version headers body
