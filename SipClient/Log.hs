module SipClient.Log where

msgLogFile :: FilePath
msgLogFile = "msg.log"

errLogFile :: FilePath
errLogFile = "err.log"

debugFile :: FilePath
debugFile = "debug.log"

eraseAllLogs :: IO ()
eraseAllLogs = mapM_
               (`writeFile` "")
               [msgLogFile, errLogFile, debugFile]

writeMsgLog :: String -> IO ()
writeMsgLog s = writeFile msgLogFile (s ++ "\n")

writeErrorLog :: String -> IO ()
writeErrorLog s = writeFile msgLogFile (s ++ "\n")

writeDebugLog :: String -> IO ()
writeDebugLog s = writeFile debugFile (s ++ "\n")
