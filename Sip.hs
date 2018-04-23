import qualified SipClient.Dialog as Dialog
import SipClient.Log
import SipClient.Types
import qualified SipClient.UdpConnection as Udp
import SipClient.UI

import Control.Concurrent
import Control.Concurrent.STM

initData :: UIData
initData = Data { numOfInCalls = 0
                , numOfOutCalls = 0 }

--comment
main :: IO ()
main = do
   uiData <- atomically $ newTVar initData
   incoming <- atomically newEmptyTMVar
   eraseAllLogs
   initUI
   _ <- forkIO $ drawUI uiData
   sock <- Udp.newSocket
   _ <- forkIO $ Dialog.idle sock incoming
   exitUI
   return ()
