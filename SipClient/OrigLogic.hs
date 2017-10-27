module SipClient.OrigLogic where

import qualified SipClient.Builder as B
import SipClient.Types
import qualified SipClient.UdpConnection as UDP
import SipClient.UI

import Control.Concurrent.STM
import Network.Socket hiding (send, sendTo, recv, recvFrom)

wait :: TVar UIData -> IO ()
wait uiData = do
  command <- getChar
  let res = case command of
              'c' -> startCall uiData
              _  -> wait uiData
  res
  return ()

startCall :: TVar UIData -> IO ()
startCall =  UDP.newSocket makeCall

defaultRecipient :: SockAddr
defaultRecipient = SockAddrUnix "localhost"

makeCall :: Socket -> TVar UIData -> IO ()
makeCall sock uiData =  do
  putStrLn "eeeee"
  let invite = return $ B.buildOutput B.newInvite
  _ <- UDP.sendMessages sock invite defaultRecipient
  refreshUI Orig INVITE uiData
  --drawUI uiData
  return ()
