module SipClient.UI where

import SipClient.Types

import Control.Monad
import Control.Concurrent.STM
import System.Process

refreshUI :: TransDirection -> ReqMethod -> TVar UIData -> IO ()
refreshUI dir rm uiData =
  case dir of
    Term -> case rm of
              INVITE -> atomically $ modifyTVar uiData addInCall
              BYE -> atomically $ modifyTVar uiData removeInCall
              _ -> return ()
    Orig -> case rm of
              INVITE -> atomically $ modifyTVar uiData addOutCall
              BYE -> atomically $ modifyTVar uiData removeOutCall
              _ -> return ()

addInCall :: UIData -> UIData
addInCall uid =  Data { numOfInCalls = numOfInCalls uid + 1
                      , numOfOutCalls = numOfOutCalls uid }

addOutCall :: UIData -> UIData
addOutCall uid =  Data { numOfInCalls = numOfInCalls uid
                      , numOfOutCalls = numOfOutCalls uid + 1}

removeInCall :: UIData -> UIData
removeInCall uid =  Data { numOfInCalls = numOfInCalls uid - 1
                      , numOfOutCalls = numOfOutCalls uid }

removeOutCall :: UIData -> UIData
removeOutCall uid =  Data { numOfInCalls = numOfInCalls uid
                      , numOfOutCalls = numOfOutCalls uid - 1}

statusLine :: UIData -> String
statusLine d = "Number of incoming calls: "
                 ++ show (numOfInCalls d)
                 ++ "\tNumber of outgoing calls: "
                 ++ show (numOfOutCalls d)

waitForChange :: UIData -> TVar UIData -> IO ()
waitForChange uid td = do
    tdFinal <- atomically $ readTVar td
    when (uid == tdFinal) $ waitForChange uid td

drawUI :: TVar UIData -> IO ()
drawUI td = do
  tdFinal <- atomically $ readTVar td
  waitForChange tdFinal td
  --callCommand "clear"
  print tdFinal
  drawUI td
