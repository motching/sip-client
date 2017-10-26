import SipClient.Log
import qualified SipClient.TermLogic as Term
import qualified SipClient.OrigLogic as Orig
import SipClient.Types
import SipClient.UI

--import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe

initData :: UIData
initData = Data { numOfInCalls = 0
                , numOfOutCalls = 0 }

main :: IO ()
main = do
   let uiData = unsafePerformIO $ newTVarIO initData
   eraseAllLogs
   _ <- forkIO $ Term.listen uiData
   drawUI uiData
   -- _ <- forkIO Orig.wait
