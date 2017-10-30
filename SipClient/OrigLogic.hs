module SipClient.OrigLogic where

import qualified SipClient.Builder as B
import qualified SipClient.TermLogic as Term
import SipClient.Types
import qualified SipClient.UdpConnection as UDP
import SipClient.UI

import qualified Data.Bits as Bit
import Data.Word
import Control.Concurrent.STM
import Network.Socket

waitForInput :: Socket -> TVar UIData -> IO ()
waitForInput sock uiData = do
  command <- getChar
  case command of
    'c' -> makeCall sock uiData
    _  -> waitForInput sock uiData

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
  _ <- UDP.sendMessages sock invite defaultRecipient
  refreshUI Orig INVITE uiData
  Term.listen sock uiData
