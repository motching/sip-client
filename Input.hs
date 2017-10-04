module Input where

--import Parser
--import StateHandling
import Types

import Network (listenOn, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

--import Control.Concurrent

sipMessage :: Connection -> IO String
sipMessage = readFile --for now

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle
    let method = head $ words line
    putStrLn "Itt vagyok, Jozsi"
    putStrLn line
    case method of
        ("INVITE") -> hPutStrLn handle "INVITE received"
        ("BYE") -> hPutStrLn handle "BYE received"
        _ -> hPutStrLn handle "Unknown command"
    commandProcessor handle

sockHandler :: Socket -> IO ()
sockHandler sock = do
    putStrLn "sockHandler"
    (handle, _, _) <- accept sock --accept is a blocking call!!!
    hSetBuffering handle NoBuffering -- flush the buffer / discard content
    --_ <- ($) forkIO commandProcessor handle
    forkIO $ commandProcessor handle --warning WTF
    sockHandler sock

sipListen :: State -> IO()
sipListen state = do
    let port = 1234
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ show port ++ " with state: " ++ show state
    sockHandler sock

-- sipListen :: State -> IO()
-- sipListen state = do
--   threadDelay 3000
--   message <- sipMessage "./message.bus"
--   let method = getRequestMethod message
--   let methodName = getMethodText method
--   putStrLn methodName
--   if newState method state
--     then putStrLn "yay, changed state!"
--     else sipListen state
