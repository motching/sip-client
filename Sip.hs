import SipClient.Log
import qualified SipClient.Transaction as Trans
import SipClient.Types
import SipClient.UdpConnection
import SipClient.UI

import Control.Concurrent
import Control.Concurrent.STM

initData :: UIData
initData = Data { numOfInCalls = 0
                , numOfOutCalls = 0 }

main :: IO ()
main = do
   uiData <- atomically $ newTVar initData
   eraseAllLogs
   initUI
   _ <- forkIO $ drawUI uiData
   sock <- newSocket
   _ <- forkIO $ Trans.listenOnUdp sock uiData
   Trans.waitForInput sock uiData
   exitUI
   return ()
