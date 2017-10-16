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

skipSpace :: AB.Parser ()
skipSpace = AB.skipWhile isSpace

parseSipMessage :: AB.Parser SipMessage
parseSipMessage = do
    reqMethod <- parseReqMethod
    uriScheme <- parseUriScheme
    reqUri <- parseReqUri
    sipVersion <- parseSipVersion

    trace ("\nreqMethod: " ++ show reqMethod) return ()
    trace ("\nuriScheme: " ++ show uriScheme) return ()
    trace ("\nreqUri: " ++ show reqUri) return ()
    trace ("\nsipVersion: " ++ show sipVersion) return ()

    --trace ("\ncallId: " ++ show callId) return ()
    trace "\nall succeed!" return ()
    return [(ReqMethod, reqMethod)]

parseReqMethod :: AB.Parser DBC.ByteString
parseReqMethod = AB.takeTill isSpace <* AB.skipWhile isSpace

parseUriScheme :: AB.Parser DBC.ByteString
parseUriScheme = AB.takeTill isColon <* AB.skipWhile isColon

parseReqUri :: AB.Parser DBC.ByteString
parseReqUri = AB.takeTill isSpace <* AB.skipWhile isSpace

parseSipVersion :: AB.Parser DBC.ByteString
parseSipVersion = AB.takeTill isEndOfLine <* AB.skipWhile isEndOfLine
