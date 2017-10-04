module Input where

import Parser
import StateHandling
import Types

import Control.Concurrent

sipMessage :: Connection -> IO String
sipMessage = readFile --for now

listen :: State -> IO()
listen state = do
  threadDelay 3000
  message <- sipMessage "./message.bus"
  let method = getRequestMethod message
  let methodName = getMethodText method
  putStrLn methodName
  if newState method state
    then putStrLn "yay, changed state!"
    else listen state
