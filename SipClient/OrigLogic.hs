module SipClient.OrigLogic where

import qualified SipClient.Builder as B
import SipClient.Types
import qualified SipClient.UdpConnection as UDP

import Control.Concurrent.STM
import Network.Socket hiding (send, sendTo, recv, recvFrom)

startCall :: TVar UIData -> IO ()
startCall = UDP.newSocket makeCall

defaultRecipient :: SockAddr
defaultRecipient = SockAddrUnix "localhost"

makeCall :: Socket -> UIData -> IO ()
makeCall sock uid = do
  let invite = return $ B.buildOutput B.newInvite
  _ <- UDP.sendMessages sock invite defaultRecipient
  return ()
