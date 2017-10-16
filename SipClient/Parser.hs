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
    reqMethod <-parseUntil isSpace
    uriScheme <- parseUntil isColon
    reqUri <- parseUntil isSpace
    sipVersion <- parseUntil isEndOfLine

    trace ("\nreqMethod: " ++ show reqMethod) return ()
    trace ("\nuriScheme: " ++ show uriScheme) return ()
    trace ("\nreqUri: " ++ show reqUri) return ()
    trace ("\nsipVersion: " ++ show sipVersion) return ()

    --trace ("\ncallId: " ++ show callId) return ()
    trace "\nall succeed!" return ()
    return [(ReqMethod, reqMethod)]
