module SipClient.Builder where

import qualified SipClient.Parser as P
import SipClient.Types

import qualified Data.ByteString.Char8 as DBC
--import Builder!

parseInput :: DBC.ByteString -> Either String SipMessage
parseInput = P.parseOnly P.parseSipMessage

checkInput :: Either String SipMessage -> SipMessage
checkInput msg = case msg of
  Right sm -> sm
  Left _ -> [(ReqMethod, DBC.pack "error geco")]

getReqMethod :: SipMessage -> DBC.ByteString
getReqMethod msg = snd
                   $ head --do we need this
                   $ filter (\h -> fst h == ReqMethod) msg

constructReply :: SipMessage -> SipMessage
constructReply msg = let
  reqMethod = getReqMethod msg
  respCode = case DBC.unpack reqMethod of
        "INVITE" -> "100"
        _        -> "505"
  answerHeader = DBC.pack $ "SIP/2.0 " ++ respCode ++ " Trying"
  -- let callId = getCallId msg
  in [(ResponseLine, answerHeader)] --why not list???

buildOutput :: SipMessage -> DBC.ByteString
buildOutput msg = snd $ head msg

answer :: DBC.ByteString -> DBC.ByteString
answer msg = buildOutput
             $ constructReply
             $ checkInput
             $ parseInput msg
