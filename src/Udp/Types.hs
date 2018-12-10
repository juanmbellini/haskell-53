module Udp.Types
    ( UdpMessage
    , Handler
    ) where


import Udp.Imports

-- | Type alias for a UDPMessage (which is no more than a ByteString).
type UdpMessage = ByteString

-- | Type alias for a function that takes an UDPMeesage,
--   and returns a Maybe UDPMessage wrapped in an IO.
--   This function is used by a UDP Server to handle a received UDPMessage,
--   indicating what should be returned (Nothing is a possible value).
type Handler = UdpMessage -> IO (Maybe UdpMessage)
