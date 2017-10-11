module SipClient.Builder where

import qualified SipClient.Parser as P
import SipClient.Types

import qualified Data.ByteString.Char8 as DBC
--import Builder!

parseInput :: DBC.ByteString -> Either String SipMessage
parseInput = P.parseOnly P.parseSipMessage

constructReply :: Either String SipMessage -> SipMessage
constructReply msg = case msg of
  Right sm -> sm
  Left _ -> [(ReqMethod, DBC.pack "error geco")]

buildOutput :: SipMessage -> DBC.ByteString
buildOutput = return $ DBC.pack "hurra"

answer :: DBC.ByteString -> DBC.ByteString
answer msg = buildOutput $ constructReply $ parseInput msg
