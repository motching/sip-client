module Main where

import Input
import Types
import Parser

main :: IO()
main = do
    message <- sipMessage "./message.bus"
    let method = getRequestMethod message
    let methodName = getMethodText method
    putStrLn methodName
