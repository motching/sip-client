module StateHandling where

import Types

newState :: RequestMethod -> State -> Bool
newState method state =
  state == Idle && method == INVITE
