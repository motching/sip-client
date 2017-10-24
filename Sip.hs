import SipClient.UdpConnection as UDP
--import SipClient.UI

import Control.Concurrent
import Control.Monad
import UI.NCurses

main :: IO()
main = do
  _ <- forkOS UDP.start
  --hey
  startUI

startUI :: IO ()
startUI =
  runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
      moveCursor 10 10
      drawString "Hey!"
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
