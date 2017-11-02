module SipClient.Dialog where

import qualified SipClient.Transaction as Trans
import SipClient.Types
import qualified SipClient.UdpConnection as Udp
import qualified SipClient.UI as UI

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as DBC
import Network.Socket (Socket)

startDialog :: TransDirection -> IO ()
startDialog dir = undefined

errorDialog :: IO ()
errorDialog = undefined

idle :: Socket -> TMVar DBC.ByteString -> IO ()
idle sock incoming = do
  _ <- forkIO $ Udp.listen sock incoming--Trans.listenOnUdp
  command <- UI.waitForInput
  case command of
    'c' -> startDialog Orig
    _   -> errorDialog --TODO error handling in general
