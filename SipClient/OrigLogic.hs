module SipClient.OrigLogic where

import qualified SipClient.Builder as B
import qualified SipClient.UdpConnection as UDP

import Network.Socket hiding (send, sendTo, recv, recvFrom)

wait :: IO ()
wait = wait

startCall :: IO ()
startCall = UDP.newSocket makeCall

defaultRecipient :: SockAddr
defaultRecipient = SockAddrUnix "localhost"

makeCall :: Socket -> IO ()
makeCall sock = do
  let invite = return $ B.buildOutput B.newInvite
  _ <- UDP.sendMessages sock invite defaultRecipient
  return ()
