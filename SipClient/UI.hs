module SipClient.UI where

import SipClient.Types

import Control.Monad
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as DBC
import System.IO
import System.Console.ANSI

--TODO: checkInput
waitForInput :: IO Char
waitForInput = do
  input <- getChar
  case input of
    'c' -> return 'c'
    _   -> waitForInput

initUI :: IO ()
initUI = do
  hSetBuffering stdin NoBuffering
  clearScreen
  --hideCursor
  --hSetEcho stdin False
  setTitle "SIP station 0.1"

exitUI :: IO ()
exitUI = do
  hSetEcho stdin True
  --clearScreen
  setCursorPosition 0 0
  setSGR [Reset]
  putStrLn "Bye!"
  showCursor

getReqMethod :: SipMessage -> ReqMethod
getReqMethod msg = case msg of
  Request {} -> getMethodType
                $ DBC.unpack
                $ reqMethod msg
  _ -> INVALID


refreshUI :: TransDirection -> SipMessage -> TVar UIData -> IO ()
refreshUI dir msg uiData =
  case dir of
    Term -> case getReqMethod msg of
              INVITE -> atomically $ modifyTVar uiData addInCall
              BYE -> atomically $ modifyTVar uiData removeInCall
              _ -> return ()
    Orig -> case getReqMethod msg of
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
  --clearScreen
  print tdFinal
  drawUI td
