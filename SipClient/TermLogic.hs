module SipClient.TermLogic where

import qualified SipClient.Builder as B
import qualified SipClient.Parser as P
import qualified SipClient.UdpConnection as UDP
import SipClient.Types

import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO.Unsafe

getNewUID :: ReqMethod -> UIData -> UIData
getNewUID rm uid = case rm of
              INVITE -> addInCall uid
              _ -> uid

addInCall :: UIData -> UIData
addInCall d = Data { numOfInCalls = 1 + numOfInCalls d
                   , numOfOutCalls = numOfOutCalls d
                   }

listen :: TVar UIData -> IO ()
listen = UDP.newSocket handleTermConnection

handleTermConnection :: Socket -> UIData -> IO ()
handleTermConnection sock uid = do
  (msg, sender) <- recvFrom sock 1024
  --TODO because we don't have state handling yet, we batch reply messages
  let parsedMsg = P.checkInput $ P.parseInput msg
  let rm = getMethodType
        $ DBC.unpack
        $ reqMethod parsedMsg
  let newUId = getNewUID rm uid
  atomically $ modifyTVar uid (getNewUID rm)
  let newUIdTVar = unsafePerformIO
                   $ newTVarIO newUId
  let replies = B.answer parsedMsg --from here it's pure
  _ <- UDP.sendMessages sock replies sender
  handleTermConnection sock newUId
