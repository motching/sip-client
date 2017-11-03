module SipClient.Dialog where

import qualified SipClient.Builder as B
import qualified SipClient.Parser as P
import qualified SipClient.Transaction as Trans
import SipClient.Types
import qualified SipClient.UdpConnection as Udp
import qualified SipClient.UI as UI

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom, listen)
import Network.Socket.ByteString

startDialog :: TransDirection -> IO ()
startDialog dir = undefined

errorDialog :: IO ()
errorDialog = undefined

idle :: Socket -> TMVar (DBC.ByteString, SockAddr) -> IO ()
idle sock incoming = do
  _ <- forkIO $ Udp.listen sock incoming--Trans.listenOnUdp
  _ <- forkIO $ processIncoming sock incoming
  command <- UI.waitForInput
  case command of
    'c' -> startDialog Orig
    _   -> errorDialog --TODO error handling in general

processIncoming :: Socket -> TMVar (DBC.ByteString, SockAddr) -> IO ()
processIncoming sock incoming = do
  (msg, sender) <- atomically $ takeTMVar incoming
  let parsedMsg = P.checkInput $ P.parseInput msg
  --UI.refreshUI dir parsedMsg uiData
  let replies = B.answer parsedMsg --from here it's pure
  Udp.sendMessages sock replies sender
