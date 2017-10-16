module SipClient.Builder where

import qualified SipClient.Parser as P
import SipClient.Types

import Debug.Trace
import qualified Data.ByteString.Char8 as DBC
--import Builder!

--move to parser?
parseInput :: DBC.ByteString -> Either String SipMessage
--parseInput m | trace ("\nparseInput: " ++ show m) False = undefined
parseInput m = P.parseOnly P.parseSipMessage m

checkInput :: Either String SipMessage -> SipMessage
checkInput m | trace ("\ncheckInput: " ++ show m) False = undefined
checkInput msg = case msg of
  Right sm -> sm
  Left _ -> [(ReqMethod, DBC.pack "error geco")]

getReqMethod :: SipMessage -> DBC.ByteString
getReqMethod msg = snd
                   $ head --do we need this
                   $ filter (\h -> fst h == ReqMethod) msg

constructReply :: SipMessage -> SipMessage
--constructReply m | trace ("\nconstructReply" ++ show m) False = undefined
constructReply msg = let
  reqMethod = getReqMethod msg
--  !debug = trace ("\nreqM: " ++ show reqMethod ++ "\n") reqMethod
  respCode = case DBC.unpack reqMethod of
        "INVITE" -> "100"
        _        -> "505"
  answerHeader = DBC.pack $ "SIP/2.0 " ++ respCode ++ " Trying"
  -- let callId = getCallId msg
  in [(RespLine, answerHeader)]

buildOutput :: SipMessage -> DBC.ByteString
buildOutput msg = snd $ head msg
answer :: DBC.ByteString -> DBC.ByteString
answer msg = buildOutput
             $ constructReply
             $ checkInput
             $ parseInput msg
