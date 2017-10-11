module SipClient.Utils where

import SipClient.Types

import qualified Data.ByteString.Char8 as DBC
import Debug.Trace

trace' :: String -> String
trace' arg = traceShow arg arg

printTuple2 :: (Header, DBC.ByteString) -> String
printTuple2 (header, content)= show header ++ ": " ++ DBC.unpack content

printValidSipMsg :: SipMessage -> IO ()
printValidSipMsg msg = putStrLn $ unlines $ map printTuple2 msg

printSipMessage :: Either String SipMessage -> IO ()
printSipMessage = either putStrLn printValidSipMsg
