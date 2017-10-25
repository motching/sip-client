module SipClient.Log where

msgLogFile :: FilePath
msgLogFile = "msg.log"

errLogFile :: FilePath
errLogFile = "err.log"

debugFile :: FilePath
debugFile = "debug.log"

eraseAllLogs :: IO ()
eraseAllLogs = mapM_
               (`appendFile` "")
               [msgLogFile, errLogFile, debugFile]

writeMsgLog :: String -> IO ()
writeMsgLog s = appendFile msgLogFile (s ++ "\n")

writeErrorLog :: String -> IO ()
writeErrorLog s = appendFile msgLogFile (s ++ "\n")

writeDebugLog :: String -> IO ()
writeDebugLog s = appendFile debugFile (s ++ "\n")
