module Main where

import SipClient.Types
import SipClient.UdpConnection as UDP

main :: IO()
main = UDP.listen Idle
