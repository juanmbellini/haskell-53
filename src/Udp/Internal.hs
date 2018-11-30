module Udp.Internal 
    ( startSocket
    , SocketMode(..)
    ) where


import Udp.Imports


data SocketMode = Server | Client
type SocketOperator = Socket -> SockAddr -> IO ()


socketOperator :: SocketMode -> SocketOperator
socketOperator Server = bind
socketOperator Client = connect


startSocket :: HostName -> ServiceName -> SocketMode -> IO (Socket)
startSocket address port mode = do
    addrinfos <- getAddrInfo Nothing (Just address) (Just port)     -- Resolve addresses into an AddrInfo instance
    let serverAddr = head addrinfos                                 -- Get only the first element in the list
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol -- Creates the Datagram socket
    let operation = socketOperator mode                             -- Gets the operation to be performed according to the given mode
    operation sock $ addrAddress serverAddr                         -- Start the socket operation
    return sock                                                     --  Return the socket
