module SipClient.UdpConnection where

import SipClient.Log

import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

newSocket :: (Socket -> IO ()) -> IO ()
newSocket handler = withSocketsDo $ do
         (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "1234")
         sock <- socket (addrFamily server) Datagram defaultProtocol
         _ <- bind sock (addrAddress server)
         writeDebugLog "Socket created ..."
         handler sock

sendMessages :: Socket -> [DBC.ByteString] -> SockAddr -> IO Int
sendMessages sock replies recipient =
  case length replies of
    0 -> return 0
    _ -> do
      _ <- logAndSend sock (head replies) recipient
      sendMessages sock (tail replies) recipient

logAndSend :: Socket -> DBC.ByteString -> SockAddr -> IO Int
logAndSend sock reply recipient = do
  writeMsgLog $ DBC.unpack reply
  sendTo sock reply recipient
