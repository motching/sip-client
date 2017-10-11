{-# LANGUAGE BangPatterns #-}

module SipClient.Builder where

import qualified SipClient.Parser as P
import SipClient.Types
import qualified SipClient.Utils as U

import qualified Data.ByteString.Char8 as DBC
--import Builder!
import Data.Either.Unwrap(fromRight)

constructReply :: DBC.ByteString -> DBC.ByteString
constructReply msg = do
  let sipMessage = P.parseOnly P.parseSipMessage msg
  let !debug = U.trace' $ show $ fromRight sipMessage
  DBC.pack "noReply"
