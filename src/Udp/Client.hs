module Udp.Client
    ( sendAndReceive
    ) where


import Udp.Imports
import Udp.Types
import Udp.Internal (startSocket, SocketMode(Client))

import System.Timeout

sendAndReceive :: HostName -> ServiceName -> Int -> UdpMessage -> IO (Maybe UdpMessage)
sendAndReceive address port waitTime req = do
    sock <- startSocket address port Client -- Start a socket in client mode
    sendAll sock req                        -- Send data
    timeout waitTime $ recv sock 4096       -- Wait till data arrives with timeout
