module SipClient.UdpConnection where

import SipClient.Log

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom, listen)
import Network.Socket.ByteString

newSocket :: IO Socket
newSocket = withSocketsDo $ do
         (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "1234")
         sock <- socket (addrFamily server) Datagram defaultProtocol
         _ <- bind sock (addrAddress server)
         return sock

sendMessages :: Socket -> [DBC.ByteString] -> SockAddr -> IO ()
sendMessages sock replies recipient =
  case length replies of
    0 -> return ()
    _ -> do
      _ <- logAndSend sock (head replies) recipient
      sendMessages sock (tail replies) recipient

logAndSend :: Socket -> DBC.ByteString -> SockAddr -> IO Int
logAndSend sock reply recipient = do
  writeMsgLog $ DBC.unpack reply
  sendTo sock reply recipient

listen :: Socket -> TMVar (DBC.ByteString, SockAddr) -> IO ()
listen sock incoming = do
  (msg, sender) <- recvFrom sock 1024 --blocking
  atomically $ putTMVar incoming (msg, sender)
  listen sock incoming

--TODO unify control constructs
choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = join $ atomically $ foldr1 orElse actions
  where
    actions :: [STM (IO ())]
    actions = [ do
                  val <- input
                  return (action val)
              | (input, action) <- choices ]
--TODO pipe?
