module Parser where

import Types

import Data.List
import Data.String

-- requestMethod :: SipMessage -> RequestMethod
-- requestMethod sm = do
--   requestLine <- head $ lines sm
--   methodText <- head $ splitOn " " requestLine
--   methodText methodType
