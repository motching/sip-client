import SipClient.Log
import qualified SipClient.TermLogic as Term
import qualified SipClient.OrigLogic as Orig
import SipClient.Types
import SipClient.UI

import Control.Concurrent

initData :: UIData
initData = Data { numOfInCalls = 0
                , numOfOutCalls = 0 }

main :: IO ()
main = do
   eraseAllLogs
   _ <- forkIO $ drawUI initData
   _ <- forkIO Term.listen
   _ <- forkIO Orig.wait
   return ()
