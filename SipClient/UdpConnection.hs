module SipClient.UdpConnection where

import qualified SipClient.Builder as B
import SipClient.Types

import qualified Control.Monad as CM
import Control.Monad.State.Lazy
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

start :: IO ()
start = withSocketsDo $ do
         (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "1234")
         sock <- socket (addrFamily server) Datagram defaultProtocol
         _ <- bind sock (addrAddress server)
         putStrLn "Server started ..."
         CM.void $ runStateT ( handleConnection sock) Idle

handleConnection :: Socket -> StateT SipState IO ()
handleConnection sock = do
  (msg, sender) <- lift $ recvFrom sock 1024 --TODO where does lift come from?
  let reply = B.answer msg --from here it's pure
  lift $ putStrLn $ DBC.unpack reply
  lift $ putStrLn "\n"
  _ <- lift $ sendTo sock reply sender --returning: number of bytes sent
  handleConnection sock
