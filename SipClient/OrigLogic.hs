module SipClient.OrigLogic where

import qualified SipClient.Builder as B
import SipClient.Types
import qualified SipClient.UdpConnection as UDP
import SipClient.UI

import qualified Data.ByteString.Char8 as DBC
import qualified Data.Bits as Bit
import Data.Word
import Control.Concurrent.STM
import Network.Socket

wait :: Socket -> TVar UIData -> IO ()
wait sock uiData = do
  command <- getChar
  case command of
    'c' -> makeCall sock uiData
    _  -> wait sock uiData

packIP :: Int -> Int -> Int -> Int -> Word32
packIP a b c d = Bit.shift (fromIntegral d) 24
                 + Bit.shift (fromIntegral c) 16
                 + Bit.shift (fromIntegral b) 8
                 + fromIntegral a

defaultRecipient :: SockAddr
defaultRecipient = SockAddrInet 5060 (packIP 127 0 0 1)

makeCall :: Socket -> TVar UIData -> IO ()
makeCall sock uiData =  do
  let invite = replicate 1 (B.buildOutput B.newInvite)
  putStrLn "poop"
  print invite
  putStrLn "peep"
  _ <- UDP.sendMessages sock invite defaultRecipient
  refreshUI Orig INVITE uiData
