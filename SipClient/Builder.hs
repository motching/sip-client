module SipClient.Builder where

import SipClient.Types

import qualified Data.ByteString.Char8 as DBC
--import Builder!

constructReply :: DBC.ByteString -> DBC.ByteString
constructReply msg = do
                let callId = "mopp" --P.parseCallId msg
                let stringMsg = concat[ "SIP/2.0 100 Trying\r\n"
                                      , "Via: SIP/2.0/UDP 200.57.7.195;branch=z9hG4bKff9b46fb055c0521cc24024da96cd290\r\n"
                                      , "From: <sip:200.57.7.195:55061;user=phone>;tag=GR52RWG346-34\r\n"
                                      , "To: \"francisco@bestel.com\" <sip:francisco@bestel.com:55060>;tag=298852044\r\n"
                                      , "Call-ID: " ++ callId ++ "\r\n"
                                      , "CSeq: 1 INVITE\r\n"
                                      ]
                DBC.pack stringMsg
