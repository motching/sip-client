module SipClient.Utils where

import SipClient.Types

import qualified Data.ByteString.Char8 as DBC

printTuple2 :: (Header, DBC.ByteString) -> String
printTuple2 (header, content)= show header ++ ": " ++ DBC.unpack content

--for debugging
printSipMessage :: SipMessage -> IO ()
printSipMessage msg = putStrLn $ unlines $ map printTuple2 msg
