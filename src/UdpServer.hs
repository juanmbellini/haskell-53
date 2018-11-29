module UdpServer
    ( startUdpServer
    , Request
    , Response
    ) where

import qualified Data.ByteString.Char8 as B
import Network.Socket hiding        (recv, recvFrom)
import Network.Socket.ByteString    (recvFrom, sendAllTo)
import Control.Monad                (forever)

type Request = B.ByteString
type Response = B.ByteString


startUdpServer :: HostName -> ServiceName -> (Request -> IO (Maybe Response)) -> IO ()
startUdpServer address port handler = do
    runUDPServer address port handler


runUDPServer :: HostName -> ServiceName -> (Request -> IO (Maybe Response)) -> IO ()
runUDPServer address port handler = do
    addrinfos <- getAddrInfo Nothing (Just address) (Just port)     -- Resolve addresses into an AddrInfo instance 
    let serveraddr = head addrinfos                                 -- Get only the first element in the list
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol -- Creates the Datagram socket 
    bind sock (addrAddress serveraddr)                              -- Bind the socket
    mainLoop sock handler                                           -- Start the server's main loop


mainLoop :: Socket -> (Request -> IO (Maybe Response)) -> IO ()
mainLoop sock handler = do
    forever $ do
        (s, addr) <- recvFrom sock 4096                             -- Receive datagram, indicating the sender
        resp <- handler $ s                                         -- Handle the request, obtaining the response
        -- TODO: should we fork for this?
        respond resp addr                                           -- Respond in case the handler returned a response
        where   respond (Just r) a  = sendAllTo sock r a
                respond Nothing _   = return ()
