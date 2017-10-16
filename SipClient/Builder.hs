module SipClient.Builder where

import qualified SipClient.Parser as P
import SipClient.Types

import Debug.Trace
import qualified Data.ByteString.Char8 as DBC
--import Builder!

--move to parser?
parseInput :: DBC.ByteString -> Either String SipMessage
--parseInput m | trace ("\nparseInput: " ++ show m) False = undefined
parseInput = P.parseOnly P.parseSipMessage

checkInput :: Either String SipMessage -> SipMessage
checkInput m | trace ("\ncheckInput: " ++ show m) False = undefined
checkInput msg =  case msg of
   Right sm -> sm
   Left _ -> BadMessage

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
             $ checkInput
             $ parseInput msg
