import SipClient.Log
import qualified SipClient.TermLogic as Term
import qualified SipClient.OrigLogic as Orig
import SipClient.Types
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
   _ <- forkIO $ Term.listen uiData
   drawUI uiData
   c <- getChar
   putChar c
   -- _ <- forkIO Orig.wait
