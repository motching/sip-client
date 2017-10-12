module SipClient.UdpConnection where

import qualified SipClient.Builder as B
import SipClient.Types

import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

listen :: State -> IO ()
listen state = withSocketsDo $ do
         (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "1234")
         s <- socket (addrFamily server) Datagram defaultProtocol
         _ <- bind s (addrAddress server)
         putStrLn "Server started ..."
         handleConnections s state

handleConnections :: Socket -> State -> IO ()
handleConnections sock state = do
  (msg, sender) <- recvFrom sock 1024
  let reply = B.answer msg
  putStrLn $ DBC.unpack reply
  _ <- sendTo sock reply sender --returning: number of bytes sent
  handleConnections sock state
