module Udp.Server 
    ( startServer
    , Handler
    ) where


import Control.Monad    (forever)

import Udp.Imports
import Udp.Types
import Udp.Internal (startSocket, SocketMode(Server))


type Handler = UdpMessage -> IO (Maybe UdpMessage)


startServer :: HostName -> ServiceName -> Handler -> IO ()
startServer = run

run :: HostName -> ServiceName -> Handler -> IO ()
run address port handler = do
    sock <- startSocket address port Server -- Start a socket in server mode
    mainLoop sock handler                   -- Start the server's main loop

mainLoop :: Socket -> Handler -> IO ()
mainLoop sock handler = do
    forever $ do
        (s, addr) <- recvFrom sock 4096                     -- Receive datagram, indicating the sender
        resp <- handler $ s                                 -- Handle the request, obtaining the response
        -- TODO: should we fork for this?
        respond resp addr                                   -- Respond in case the handler returned a response
        where
            respond :: Maybe UdpMessage -> SockAddr -> IO ()
            respond (Just r) a  = sendAllTo sock r a
            respond Nothing _   = return ()
