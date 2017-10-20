module SipClient.UdpConnection where

import qualified SipClient.Builder as B
import SipClient.Types

import qualified Control.Monad as CM
--import Control.Monad.State.Lazy
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

start :: IO ()
start = withSocketsDo $ do
         (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "1234")
         sock <- socket (addrFamily server) Datagram defaultProtocol
         _ <- bind sock (addrAddress server)
         putStrLn "Server started ..."
         --CM.void $ runStateT (handleConnection sock) Idle
         handleConnection sock

sendMessage :: Socket -> DBC.ByteString -> SockAddr -> IO Int
sendMessage sock reply sender = do
  putStrLn $ DBC.unpack reply
  putStrLn "\n"
  sendTo sock reply sender

-- mergeStates :: [StateT SipState IO Int] -> StateT SipState IO Int
-- mergeStates states = head states

handleConnection :: Socket -> IO ()
handleConnection sock = do
  (msg, sender) <- recvFrom sock 1024 --TODO where does lift come from?
  --TODO because we don't have state handling yet, we batch reply messages
  let replies = B.answer msg --from here it's pure
  _ <- head $ map (\r -> sendMessage sock r sender) replies
--  _ <- sendMessage sock replies sender  --returning: number of bytes sent
  handleConnection sock
