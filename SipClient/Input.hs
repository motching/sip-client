module SipClient.Input where

import SipClient.Parser
--import StateHandling
import SipClient.Types

import Network (listenOn, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    message <- hGetLine handle
    let method = getRequestMethod message
   -- let methodName = getMethodText method
    case method of
        INVITE -> hPutStrLn handle "INVITE received"
        BYE -> hPutStrLn handle "BYE received"
        _ -> hPutStrLn handle "Unknown command"
    commandProcessor handle

sockHandler :: Socket -> IO ()
sockHandler sock = do
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
