module SipClient.UdpConnection where

--import SipClient.Parser
import SipClient.Types

import qualified Data.ByteString as DB-- hiding (putStrLn)
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

listen :: State -> IO ()
listen state = withSocketsDo $ do
         (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "1234")
         s <- socket (addrFamily server) Datagram defaultProtocol
         _ <- bind s (addrAddress server) >> return s
         putStrLn "Server started ..."
         handleConnections s state

constructReply :: DB.ByteString -> DB.ByteString
constructReply msg = do
                let stringMsg = concat[ "SIP/2.0 100 Trying\r\n"
                                      , "Via: SIP/2.0/UDP 200.57.7.195;branch=z9hG4bKff9b46fb055c0521cc24024da96cd290\r\n"
                                      , "Via: SIP/2.0/UDP 200.57.7.195:55061;branch=z9hG4bK291d90e31a47b225bd0ddff4353e9cc0\r\n"
                                      , "From: <sip:200.57.7.195:55061;user=phone>;tag=GR52RWG346-34\r\n"
                                      , "To: \"francisco@bestel.com\" <sip:francisco@bestel.com:55060>;tag=298852044\r\n"
                                      , "Contact: <sip:francisco@200.57.7.204:5061>\r\n"
                                      , "Call-ID: 12013223@200.57.7.195\r\n"
                                      , "CSeq: 1 INVITE\r\n"
                                      , "Server: X-Lite release 1103m\r\n"
                                      , "Content-Length: 0\r\n"
                                      ]
                DBC.pack stringMsg

handleConnections :: Socket -> State -> IO ()
handleConnections sock state = do
  (text, sender) <- recvFrom sock 1024
  DBC.putStrLn text
  let reply = constructReply text
  _ <- sendTo sock reply sender
  handleConnections sock state
