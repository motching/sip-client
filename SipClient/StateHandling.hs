module SipClient.StateHandling where

import SipClient.Types

newState :: RequestMethod -> State -> Bool
newState method state =
  state == Idle && method == INVITE
