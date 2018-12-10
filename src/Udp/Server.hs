module Udp.Server where


import Control.Monad    (forever)

import Udp.Imports
import Udp.Types
import Udp.Internal (startSocket, SocketMode(Server))


-- | Starts a UDP Server binded to the given address (the HostName parameter),
-- | and listening in the given port (the ServiceName parameter).
--   The given Handler is used to handle a received UDPMessage, and should indicate
--   what must be returned after processing it (can be Nothing).
startServer :: HostName -> ServiceName -> Handler -> IO ()
startServer address port handler = do
    sock <- startSocket address port Server -- Start a socket in server mode
    mainLoop sock handler                   -- Start the server's main loop

-- | The servers main loop.
--   Given a Socket and a Handler, run the UDO server's main loop,
--   receiving data from the Socket (blocking operation), handling the received data,
--   and sending the handling result back.
mainLoop :: Socket -> Handler -> IO ()
mainLoop sock handler = do
    forever $ do
        (s, addr) <- recvFrom sock 4096                     -- Receive datagram, indicating the sender
        resp <- handler $ s                                 -- Handle the request, obtaining the response
        -- TODO: should we fork for this?
        respond addr resp                                   -- Respond in case the handler returned a response
        where
            -- | Function that sends back the given UdpMessage to the given SockAddr.
            --   If the given UdpMessage is not present, nothing is done.
            respond :: SockAddr -> Maybe UdpMessage -> IO ()
            respond a (Just r)  = sendAllTo sock r a
            respond _ Nothing   = return ()
