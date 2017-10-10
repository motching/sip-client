module SipClient.Builder where

import qualified SipClient.Parser as P
--import SipClient.Types

import qualified Data.ByteString.Char8 as DBC
--import Builder!

constructReply :: DBC.ByteString -> DBC.ByteString
constructReply msg = do
  let sipMessage = P.parseOnly P.parseSipMessage msg
  --either putStrLn DBC.putStrLn method
  DBC.pack "noReply"
