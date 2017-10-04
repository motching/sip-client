module SipClient.Parser where

import SipClient.Types

import Data.List.Split

getRequestMethod :: SipMessage -> RequestMethod
getRequestMethod sm = do
  let requestLine = head $ lines sm
  let method = head $ splitOn " " requestLine
  getMethodType method
