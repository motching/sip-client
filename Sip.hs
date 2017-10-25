import SipClient.Log
import SipClient.UdpConnection as UDP
import SipClient.UI as UI

import Control.Concurrent

main :: IO ()
main = do
   eraseAllLogs
   _ <- forkIO UI.startUI
   UDP.start
