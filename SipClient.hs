module Main where

import SipClient.Input
import SipClient.Types

main :: IO()
main = sipListen Idle
