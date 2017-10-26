module SipClient.UI where

import SipClient.Types

import Control.Concurrent.STM
import Control.Monad
import System.IO.Unsafe
import UI.NCurses

statusLine :: IO UIData -> String
statusLine iod = do
  let d = unsafePerformIO iod
  "Number of incoming calls: "
    ++ show (numOfInCalls d)
    ++ "\tNumber of outgoing calls: "
    ++ show (numOfOutCalls d)

drawUI :: TVar UIData -> IO ()
drawUI td =
   runCurses
  $ do
    let d = readTVarIO td
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
      moveCursor 10 10
      drawString $ statusLine d
      moveCursor 12 10
      drawString "c to place a call"
    render
    waitFor w (\ev -> ev == EventCharacter 'c')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> unless (p ev') loop
