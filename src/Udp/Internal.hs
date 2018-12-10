module Udp.Internal where


import Udp.Imports


-- | Socket operation modes
data SocketMode = Server    -- ^ Indicates that the socket should act as a server.
                | Client    -- ^ Indicates that the socket should act as a client.

-- | Type alias for a function that takes a Socket and a SockAddr,
--   and performs an IO Action
type SocketOperator = Socket -> SockAddr -> IO ()

-- | Function that takes a SocketMode, and returns the SocketOperation that corresponds to it.
socketOperator :: SocketMode -> SocketOperator
socketOperator Server = bind
socketOperator Client = connect

-- | Starts a UDP Socket, returning it in an IO.
--   The socket mode indicates how the HostName and ServiceName should be used.
--   If the SocketMode is Server, then it binds the socket to the addres and port (to receive data).
--   If the SocketMode is Client, then it connects to the address and port (to send data).
startSocket :: HostName -> ServiceName -> SocketMode -> IO (Socket)
startSocket address port mode = do
    addrinfos <- getAddrInfo Nothing (Just address) (Just port)     -- Resolve addresses into an AddrInfo instance
    let serverAddr = head addrinfos                                 -- Get only the first element in the list
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol -- Creates the Datagram socket
    let operation = socketOperator mode                             -- Gets the operation to be performed according to the given mode
    operation sock $ addrAddress serverAddr                         -- Start the socket operation
    return sock                                                     --  Return the socket
