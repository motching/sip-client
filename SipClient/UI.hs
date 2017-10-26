module SipClient.UI where

import SipClient.Types

import Control.Concurrent.STM

refreshUI :: ReqMethod -> TVar UIData -> IO ()
refreshUI rm uiData = case rm of
                        INVITE -> atomically $ modifyTVar uiData addInCall
                        _ -> return ()

addInCall :: UIData -> UIData
addInCall uid =  Data { numOfInCalls = 1 + numOfInCalls uid
                      , numOfOutCalls = numOfOutCalls uid }

addOutCall :: UIData -> UIData
addOutCall uid =  Data { numOfInCalls = numOfInCalls uid
                      , numOfOutCalls = 1 + numOfOutCalls uid }

statusLine :: UIData -> String
statusLine d = "Number of incoming calls: "
                 ++ show (numOfInCalls d)
                 ++ "\tNumber of outgoing calls: "
                 ++ show (numOfOutCalls d)

drawUI :: TVar UIData -> IO ()
drawUI d = do
  uid <- atomically $ readTVar d
  putStrLn $ statusLine uid
