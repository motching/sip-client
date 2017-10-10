module SipClient.UdpConnection where

import qualified SipClient.Builder as B
import qualified SipClient.Parser as P
import SipClient.Types

import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

listen :: State -> IO ()
listen state = withSocketsDo $ do
         (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "1234")
         s <- socket (addrFamily server) Datagram defaultProtocol
         _ <- bind s (addrAddress server) >> return s
         putStrLn "Server started ..."
         handleConnections s state

handleConnections :: Socket -> State -> IO ()
handleConnections sock state = do
  (msg, sender) <- recvFrom sock 1024
  let method = P.parseOnly P.parseReqMethod msg
  putStrLn "here"
  either putStrLn DBC.putStrLn method
  putStrLn "there"
  let reply = B.constructReply msg
  _ <- sendTo sock reply sender
  handleConnections sock state
