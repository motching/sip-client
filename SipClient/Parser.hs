module SipClient.Parser where

import Prelude hiding (takeWhile)

import SipClient.Types

import Data.Word8 hiding (isSpace)
import Debug.Trace
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.ByteString.Char8 as DBC

checkInput :: Either String SipMessage -> SipMessage
checkInput m | trace ("\ncheckInput: " ++ show m) False = undefined
checkInput msg =  case msg of
   Right sm -> sm
   Left _ -> BadMessage

parseInput :: DBC.ByteString -> Either String SipMessage
parseInput = AB.parseOnly parseSipMessage

isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 13 || w == 10   -- CR or LF

isSpace :: Word8 -> Bool
isSpace w = w == 32 || w == 9        --space or horizontal tab

isColon :: Word8 -> Bool
isColon w = w == 58                  -- colon

parseTill :: (Word8 -> Bool) -> AB.Parser DBC.ByteString
parseTill cond = AB.takeTill cond <* AB.skipWhile cond

parseHeader :: AB.Parser Header
parseHeader = do
  hname <- parseTill isColon --TODO isColon || isSpace..some functor stuff
  _ <- parseTill isSpace
  hvalue <- AB.takeTill isEndOfLine
            <* AB.skip isEndOfLine
            <* AB.skip isEndOfLine --TODO this is a bad hack, consume \r\n
  return (getHeaderNameFromText hname, hvalue)

parseHeaders :: AB.Parser [Header]
parseHeaders =  AC.manyTill parseHeader (AB.string (DBC.pack "\r\n"))
           --     <* AB.string (DBC.pack "\r\n")

parseSipMessage :: AB.Parser SipMessage
parseSipMessage = do
    method <-parseTill isSpace
    scheme <- parseTill isColon
    uri <- parseTill isSpace
    version <- parseTill isEndOfLine

    -- trace ("\nreqMethod: " ++ show method) return ()
    -- trace ("\nuriScheme: " ++ show scheme) return ()
    -- trace ("\nreqUri: " ++ show uri) return ()
    -- trace ("\nsipVersion: " ++ show version) return ()

    hdrs <- parseHeaders
    b <- AB.takeByteString

    -- trace ("\ncallId: " ++ show callId) return ()
    trace "\nall succeed!" return ()

    return Request { reqMethod = method
                   , uriScheme = scheme
                   , reqUri = uri
                   , sipVersion = version
                   , headers = hdrs
                   , body = b
                    }

--TODO more elegant syntax for naming record fields?

    -- let headers = [(CallId, DBC.pack "call-id")]
    -- let body = DBC.pack "body"

    -- return Request method scheme uri version headers body
