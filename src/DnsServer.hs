module DnsServer
    ( startDnsServer
    ) where

import Lib
import UdpServer
import Network.Socket       (HostName, ServiceName)
import Network.DNS.Decode   (decode)
import Network.DNS.Encode   (encode)
import Network.DNS.Types
import Data.IP              (toIPv4)


startDnsServer :: HostName -> ServiceName -> IO ()
startDnsServer address port = do
    startUdpServer address port handleDns

handleDns :: UdpServer.Request -> IO (Maybe UdpServer.Response)
handleDns = (=<<) encodeRequest . (=<<) handleDnsRequest . decodeRequest -- Decode binary to DNS, then handle DNS, then encode DNS to binary


-- ================================================================================================
-- Decoding
-- ================================================================================================

decodeRequest :: UdpServer.Request -> IO (Maybe DNSMessage)
decodeRequest req = do
    either
        (\_ -> (putStrLn "An unknown message was received." >>= \_ -> return Nothing))  -- Log and do nothing if cannot decode
        toIOJust                                                                        -- Wrap the DNSMessage into an IO (Just DNSMessage)
        (decode req)                                                                    -- Decode the binary request (this happens first)


-- ================================================================================================
-- Handling
-- ================================================================================================

handleDnsRequest :: Maybe DNSMessage -> IO (Maybe DNSMessage)
handleDnsRequest dnsReq = do
    maybe 
        (return Nothing)                            -- If not present, return IO Nothing
        ((=<<) toIOJust . handleDnsQueryMessage)    -- If present, handle the dns query, then wrap into an IO (Just DNSMessage)
        dnsReq

handleDnsQueryMessage :: DNSMessage -> IO DNSMessage
handleDnsQueryMessage queryMessage = do
    putStrLn "A DNS message was received"
    let queryHeader = header queryMessage
    let queryIdentifier = identifier queryHeader
    let queryFlags = flags queryHeader
    -- TODO: discard if QR is QR_Response
    let queryQuestion = head . question $ queryMessage

    -- let responseHeader
    let dnsResponose = DNSMessage {
        header = DNSHeader {
            identifier = queryIdentifier,
            flags = DNSFlags {
                qOrR            = QR_Response,
                opcode          = opcode queryFlags,
                authAnswer      = False,
                trunCation      = False,
                recDesired      = recDesired queryFlags,
                recAvailable    = False,
                rcode           = NoErr,
                authenData      = True
            }
        },
        question = [queryQuestion], -- TODO: check if copy

        answer = [
            ResourceRecord {
                rrname  = qname queryQuestion,
                rrtype  = A,
                rrclass = classIN,
                rrttl   = 300,
                rdata   = RD_A . toIPv4 $ [8, 8, 8, 8]
            },
            ResourceRecord {
                rrname  = qname queryQuestion,
                rrtype  = A,
                rrclass = classIN,
                rrttl   = 300,
                rdata   = RD_A . toIPv4 $ [8, 8, 4, 4]
            }
        ],
        authority = [],
        additional = []
    }
    -- TODO: query database for record
    return dnsResponose


-- ================================================================================================
-- Encoding
-- ================================================================================================

encodeRequest :: Maybe DNSMessage -> IO (Maybe UdpServer.Response)
encodeRequest dnsResp = do
    maybe
        (return Nothing)    -- If not present, return IO Nothing
        (toIOJust . encode) -- If present, encode DNSMessage into binary, then wrap into an IO (Just DNSMessage)
        dnsResp
