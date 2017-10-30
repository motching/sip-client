module SipClient.TermLogic where

import qualified SipClient.Builder as B
import qualified SipClient.Parser as P
import qualified SipClient.UdpConnection as UDP
import SipClient.Types
import SipClient.UI

import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as DBC
import Network.Socket (Socket)
import Network.Socket.ByteString

getNewUID :: ReqMethod -> UIData -> UIData
getNewUID rm uid = case rm of
              INVITE -> addInCall uid
              _ -> uid

listen :: Socket -> TVar UIData -> IO ()
listen sock uiData = do
  (msg, sender) <- recvFrom sock 1024
  --TODO because we don't have state handling yet, we batch reply messages
  let parsedMsg = P.checkInput $ P.parseInput msg
  let rm = getMethodType
        $ DBC.unpack
        $ reqMethod parsedMsg
  refreshUI Term rm uiData
  let replies = B.answer parsedMsg --from here it's pure
  _ <- UDP.sendMessages sock replies sender
  listen sock uiData
