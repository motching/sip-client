module Main where

import Types
--import Parser

import Data.List.Split

sipMessage :: Connection -> IO String
sipMessage = readFile --for now

requestMethod :: SipMessage -> RequestMethod
requestMethod sm = do
  let requestLine = head $ lines sm
  let method = head $ splitOn " " requestLine
  getMethodType method

main :: IO()
main = do
    message <- sipMessage "./message.bus"
    let method = requestMethod message
    let methodName = getMethodText method
    putStrLn methodName
