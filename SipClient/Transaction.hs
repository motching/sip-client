module SipClient.Transaction where

import qualified SipClient.Builder as B
import qualified SipClient.Parser as P
import SipClient.Types
import qualified SipClient.UdpConnection as UDP
import SipClient.UI

import qualified Data.Bits as Bit
import Data.Word
import Control.Concurrent.STM
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

newTransaction :: Dialog -> ReqMethod -> TransDirection -> IO Dialog
newTransaction currentDlg rm dir = undefined

waitForInput :: Socket -> TVar UIData -> IO ()
waitForInput sock uiData = do
  command <- getChar
  case command of
    'b' -> stopCall sock uiData
    'c' -> startCall sock uiData
    _  -> waitForInput sock uiData

packIP :: Int -> Int -> Int -> Int -> Word32
packIP a b c d = Bit.shift (fromIntegral d) 24
                 + Bit.shift (fromIntegral c) 16
                 + Bit.shift (fromIntegral b) 8
                 + fromIntegral a

defaultRecipient :: SockAddr
defaultRecipient = SockAddrInet 5060 (packIP 127 0 0 1)

startCall :: Socket -> TVar UIData -> IO ()
startCall sock uiData =  do
  let invite = B.newInvite
  let rawInvite = replicate 1 $ B.buildOutput invite
  _ <- UDP.sendMessages sock rawInvite defaultRecipient
  refreshUI Orig invite uiData
  listenOnUdp Orig sock uiData

stopCall :: Socket -> TVar UIData -> IO ()
stopCall sock uiData = do
  let bye = B.newBye
  let rawBye = replicate 1 $ B.buildOutput bye
  _ <- UDP.sendMessages sock rawBye defaultRecipient
  refreshUI Orig bye uiData
  waitForInput sock uiData

getNewUID :: ReqMethod -> UIData -> UIData
getNewUID rm uid = case rm of
              INVITE -> addInCall uid
              _ -> uid

listenOnUdp :: TransDirection -> Socket -> TVar UIData -> IO ()
listenOnUdp dir sock uiData = do
  (msg, sender) <- recvFrom sock 1024
  --TODO because we don't have state handling yet, we batch reply messages
  let parsedMsg = P.checkInput $ P.parseInput msg
  refreshUI dir parsedMsg uiData
  let replies = B.answer parsedMsg --from here it's pure
  _ <- UDP.sendMessages sock replies sender
  listenOnUdp dir sock uiData
