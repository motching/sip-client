module SipClient.TermLogic where

import qualified SipClient.Builder as B
import qualified SipClient.UdpConnection as UDP

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString


listen :: IO ()
listen = UDP.newSocket handleTermConnection

handleTermConnection :: Socket -> IO ()
handleTermConnection sock = do
  (msg, sender) <- recvFrom sock 1024
  --TODO because we don't have state handling yet, we batch reply messages
  let replies = B.answer msg --from here it's pure
  _ <- UDP.sendMessages sock replies sender
  handleTermConnection sock
