module SipClient.Builder where

import SipClient.Types

import Debug.Trace
import qualified Data.ByteString.Char8 as DBC
--TODO import Builder? probably overkill

assembleHeaders :: [Header] -> [Header]
assembleHeaders hdrs = hdrs
--assembleHeaders hdrs = [(Via, DBC.pack "oops")]--hdrs

newInvite :: SipMessage
newInvite = Request { reqMethod = DBC.pack "INVITE"
                    , uriScheme = DBC.pack "sip"
                    , reqUri = DBC.pack "bela@kocsma.hu"
                    , sipVersion = DBC.pack "SIP/2.0"
                    , headers = [(CallId, DBC.pack "sipstation@127.0.0.1")
                                ,(CSeq, DBC.pack "1 INVITE")]
                    , body = DBC.pack "body"
                    }

newAck :: SipMessage
newAck = Request { reqMethod = DBC.pack "ACK"
                    , uriScheme = DBC.pack "sip"
                    , reqUri = DBC.pack "bela@kocsma.hu"
                    , sipVersion = DBC.pack "SIP/2.0"
                    , headers = [(CallId, DBC.pack "sipstation@127.0.0.1")
                                ,(CSeq, DBC.pack "2 ACK")]
                    , body = DBC.pack "body"
                    }

new100Trying :: SipMessage -> SipMessage
new100Trying  msg = Response { sipVersion = sipVersion msg
              , statusCode = 100
              , reasonPhrase = DBC.pack "Trying"
              , headers = assembleHeaders $ headers msg
              , body = DBC.pack ""
              }

new180Ringing :: SipMessage -> SipMessage
new180Ringing  msg = Response { sipVersion = sipVersion msg
              , statusCode = 180
              , reasonPhrase = DBC.pack "Ringing"
              , headers = assembleHeaders $ headers msg
              , body = DBC.pack ""
              }

new183SessionProgress :: SipMessage -> SipMessage
new183SessionProgress  msg = Response { sipVersion = sipVersion msg
              , statusCode = 183
              , reasonPhrase = DBC.pack "Session Progress"
              , headers = assembleHeaders $ headers msg
              , body = DBC.pack ""
              }

new200OK :: SipMessage -> SipMessage
new200OK  msg = Response { sipVersion = sipVersion msg
              , statusCode = 200
              , reasonPhrase = DBC.pack "OK"
              , headers = assembleHeaders $ headers msg
              , body = DBC.pack ""
              }


constructReply :: SipMessage -> [SipMessage]
constructReply m | trace (show m ++ "\n") False = undefined
constructReply msg = case msg of
  Request {} -> let
    method = reqMethod msg
    ans = case DBC.unpack method of
        "INVITE" -> [new100Trying msg
                    ,new180Ringing msg
                    ,new183SessionProgress msg
                    ,new200OK msg
                    ]
        "BYE" -> [new200OK msg]
        _        -> []
    in ans
  Response {} -> let
    scode = statusCode msg
    ans = case scode of
      200 -> [newAck]
      _ -> []
    in ans
  _ -> [BadMessage]


showHeader :: Header -> DBC.ByteString
showHeader hdr = DBC.pack  (getHeaderText (fst hdr))
                 `DBC.append` DBC.pack ":"
                 `DBC.append` snd hdr

buildOutput :: SipMessage -> DBC.ByteString
buildOutput msg = case msg of

  Response {} -> let
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
       `DBC.append`  newl
       `DBC.append`  newl
       `DBC.append`  body msg

  Request {} -> let
    newl = DBC.pack "\r\n"
    hdrtxt = fmap showHeader (headers msg)
    hdrs = DBC.intercalate newl hdrtxt
    in reqMethod msg
       `DBC.append`  DBC.pack " "
       `DBC.append`  uriScheme msg
       `DBC.append`  DBC.pack ":"
       `DBC.append`  reqUri msg
       `DBC.append`  DBC.pack ":"
       `DBC.append`  sipVersion msg
       `DBC.append`  newl
       `DBC.append`  hdrs
       `DBC.append`  newl
       `DBC.append`  newl
       `DBC.append`  body msg

  BadMessage -> DBC.pack "bad message!"

answer :: SipMessage -> [DBC.ByteString]
answer msg = map buildOutput
             $ constructReply msg

  --TODO: default bytestrings
