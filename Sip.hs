import SipClient.Log
import SipClient.Types
import qualified SipClient.UdpConnection as UDP
import SipClient.UI

import Control.Concurrent

initData :: UIData
initData = Data { numOfInCalls = 0
                , numOfOutCalls = 0 }

main :: IO ()
main = do
   eraseAllLogs
   _ <- forkIO $ drawUI initData
   UDP.start
