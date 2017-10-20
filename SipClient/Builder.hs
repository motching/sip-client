module SipClient.Builder where

import qualified SipClient.Parser as P
import SipClient.Types

import Debug.Trace
import qualified Data.ByteString.Char8 as DBC
--TODO import Builder? probably overkill

assembleHeaders :: [Header] -> [Header]
assembleHeaders hdrs = hdrs

new100Trying :: SipMessage -> SipMessage
new100Trying  msg = Response { sipVersion = sipVersion msg
              , statusCode = 100
              , reasonPhrase = DBC.pack "Trying"
              , headers = assembleHeaders $ headers msg
              , body = DBC.pack ""
              }


constructReply :: SipMessage -> [SipMessage]
constructReply m | trace (show m ++ "\n") False = undefined
constructReply msg = let
  method = reqMethod msg
  ans = case DBC.unpack method of
        "INVITE" -> [new100Trying msg]
        _        -> []
  in ans

showHeader :: Header -> DBC.ByteString
showHeader hdr = DBC.pack  (getHeaderText (fst hdr))
                 `DBC.append` DBC.pack ":"
                 `DBC.append` snd hdr

buildOutput :: SipMessage -> DBC.ByteString
buildOutput msg = let
  --TODO clean up this mess
  newl = DBC.pack "\r\n"
  hdrtxt = fmap showHeader (headers msg)
  hdrs = DBC.intercalate newl hdrtxt

  in sipVersion msg
     `DBC.append`  DBC.pack " "
     `DBC.append`  DBC.pack (show (statusCode msg))
     `DBC.append`  DBC.pack " "
     `DBC.append`  reasonPhrase msg
     `DBC.append`  newl
     `DBC.append`  hdrs

answer :: DBC.ByteString -> [DBC.ByteString]
answer msg = map buildOutput
             $ constructReply
             $ P.checkInput  --TODO move to parser?
             $ P.parseInput msg

  --TODO: default bytestrings
