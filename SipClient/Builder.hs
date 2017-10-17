module SipClient.Builder where

import qualified SipClient.Parser as P
import SipClient.Types

import Debug.Trace
import qualified Data.ByteString.Char8 as DBC
--import Builder!

constructReply :: SipMessage -> SipMessage
--constructReply m | trace ("\nconstructReply" ++ show m) False = undefined
constructReply msg = let

  -- reqMethod = case msg of
   --   Request req _ _ _ _ _-> reqMethod req
   --             _ -> (DBC.pack "Bad request")

  method = reqMethod msg

  respCode = case DBC.unpack method of
        "INVITE" -> 100
        _        -> 505

  in Response { sipVersion = DBC.pack  "SIP/2.0"
                  , statusCode = respCode
                  , reasonPhrase = DBC.pack  "Trying"
                  , headers = [(CallId, DBC.pack "call-id")]
                  , body = DBC.pack "body"
                  }

buildOutput :: SipMessage -> DBC.ByteString
buildOutput msg = DBC.pack $ show msg--DBC.pack "temp"
  --snd $ head msg

answer :: DBC.ByteString -> DBC.ByteString
answer msg = buildOutput
             $ constructReply
             $ P.checkInput  --TODO move to parser?
             $ P.parseInput msg
