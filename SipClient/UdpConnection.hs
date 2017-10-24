module SipClient.UdpConnection where

import qualified SipClient.Builder as B
--import SipClient.Types

--import qualified Control.Monad as CM
--import Control.Monad.State.Lazy
import Control.Concurrent
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

start :: IO ()
start = withSocketsDo $ do
         (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "1234")
         sock <- socket (addrFamily server) Datagram defaultProtocol
         _ <- bind sock (addrAddress server)
         putStrLn "Server started ..."
         handleConnection sock

printAndSend :: Socket -> DBC.ByteString -> SockAddr -> IO Int
printAndSend sock reply sender = do
  putStrLn $ DBC.unpack reply
  putStrLn "\n"
  sendTo sock reply sender

sendMessages :: Socket -> [DBC.ByteString] -> SockAddr -> IO Int
sendMessages sock replies sender =
  case length replies of
    0 -> return 0
    _ -> do
      _ <- printAndSend sock (head replies) sender
      sendMessages sock (tail replies) sender

handleConnection :: Socket -> IO ()
handleConnection sock = do
  (msg, sender) <- recvFrom sock 1024
  --TODO because we don't have state handling yet, we batch reply messages
  let replies = B.answer msg --from here it's pure
  _ <- sendMessages sock replies sender
  handleConnection sock
