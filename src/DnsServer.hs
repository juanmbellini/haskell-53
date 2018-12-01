module DnsServer
    ( DnsServerConfig(..)
    , startDnsServer
    ) where

import Lib
-- import Udp

-- import Udp.Server           (start)
-- import qualified Udp.Types as Udp

import qualified Udp.Server as US
import qualified Udp.Client as UC
import Udp.Types

import Network.Socket       (HostName, ServiceName)
import Network.DNS.Decode   (decode)
import Network.DNS.Encode   (encode)
import Network.DNS.Types
-- import Data.IP              (toIPv4)
import Data.Function        (on)

type NameServers = [HostName]
type ErrorHandler = DNSError -> IO ()
type ErrorCreator = DNSMessage -> DNSMessage


startDnsServer :: DnsServerConfig -> IO ()
startDnsServer config = do
    US.startServer address port $ handleDns servers
    where
        address = listeningAddress config
        port    = listeningPort config
        servers = nameServers config

handleDns :: NameServers -> UdpMessage -> IO (Maybe UdpMessage)
handleDns servers = (=<<) encodeResponse . (=<<) (handleDnsRequest servers) . decodeRequest -- Decode binary to DNS, then handle DNS, then encode DNS to binary


-- ================================================================================================
-- Encoding / Decoding
-- ================================================================================================

encodeResponse :: Maybe DNSMessage -> IO (Maybe UdpMessage)
encodeResponse dnsResp = do
    maybe
        (return Nothing)    -- If not present, return IO Nothing
        (toIOJust . encode) -- If present, encode DNSMessage into binary, then wrap into an IO (Just UdpMessage)
        dnsResp

decodeRequest :: UdpMessage -> IO (Maybe DNSMessage)
decodeRequest = flip decodeUdpMessage (\_ -> putStrLn "An unknown request message was received.")

decodeResponse :: UdpMessage -> IO (Maybe DNSMessage)
decodeResponse = flip decodeUdpMessage (\_ -> putStrLn "An unknown response message was received.")

decodeUdpMessage :: UdpMessage -> ErrorHandler -> IO (Maybe DNSMessage)
decodeUdpMessage msg errorHandler = do
    either
        ((=<<) (\_ -> return Nothing) . errorHandler)   -- Handle error and return Nothing
        toIOJust                                        -- Wrap the DNSMessage into an IO (Just DNSMessage)
        (decode msg)                                    -- Decode the binary request (this happens first)


-- ================================================================================================
-- Handling
-- ================================================================================================

handleDnsRequest :: NameServers -> Maybe DNSMessage -> IO (Maybe DNSMessage)
handleDnsRequest servers dnsReq = do
    maybe 
        (return Nothing)                                    -- If not present, return IO Nothing
        ((=<<) toIOJust . handleDnsQueryMessage servers)    -- If present, handle the dns query, then wrap into an IO (Just DNSMessage)
        dnsReq


analyzeForErrors :: DNSMessage -> IO (Maybe ErrorCreator)
analyzeForErrors msg = do
    let queryHeader = header msg
        queryFlags = flags queryHeader
        qr = qOrR queryFlags
        oc = opcode queryFlags
    if validQR qr
        then if validOpcde oc
            then return Nothing
            else return (Just createNotImplementedError)
        else return (Just createFormatError)
    where
        validQR :: QorR -> Bool
        validQR = (==) QR_Query

        validOpcde :: OPCODE -> Bool
        validOpcde = flip elem [OP_STD] -- As a list because in the future more opcodes can be supported


handleDnsQueryMessage :: NameServers -> DNSMessage -> IO DNSMessage
handleDnsQueryMessage servers queryMessage = do
    putStrLn "A DNS message was received"
    errorCreator <- analyzeForErrors queryMessage
    maybe handleQuery sendError errorCreator
    where
        sendError :: ErrorCreator -> IO DNSMessage
        sendError = flip returnError queryMessage

        handleQuery :: IO DNSMessage
        handleQuery =
            -- First check if it is a managed zone
            -- If yes, return the answer according the information
            -- If not, then
            --      Check cache
            --      If data available and not expired in cache, then return
            --      If data not present or expired, then forward or recurse
            forwardRequest queryMessage servers -- TODO: check forward or recurse
            -- TODO: cache result (add if not present, update if expired)


forwardRequest :: DNSMessage -> NameServers -> IO DNSMessage
forwardRequest dnsReq servers = do
    resp <- UC.sendAndReceive (head servers) "53" 100000 $ encode dnsReq -- TODO: retry? use another name server?
    maybe
        sendError           -- In case of not getting a response from nameservers, return server error
        handleUdpMessage    -- If response was received, handle it.
        resp
    where
        sendError :: IO DNSMessage
        sendError = returnError createServerError dnsReq            -- Create the server error from the query

        identifierVerification :: DNSMessage -> IO DNSMessage
        identifierVerification resp = do
            if sameIdentifier dnsReq resp                           -- Check if the identifier is the same
                then return resp                                    -- Return response if yes
                else sendError                                      -- Return error if not

        handleDnsMessage :: Maybe DNSMessage -> IO DNSMessage
        handleDnsMessage = maybe sendError identifierVerification   -- If DNSMessage, then perform identifier verification. Else send error

        handleUdpMessage :: UdpMessage -> IO DNSMessage
        handleUdpMessage = (=<<) handleDnsMessage . decodeResponse  -- Decode the UdpMessage and try to handle it as DNS



-- ================================================================================================
-- Helpers
-- ================================================================================================

createFormatError :: ErrorCreator
createFormatError = createError FormatErr

createServerError :: ErrorCreator
createServerError = createError ServFail

createNotImplementedError :: ErrorCreator
createNotImplementedError = createError NotImpl


createError :: RCODE -> ErrorCreator
createError rc queryMessage = DNSMessage {
    header      = responseHeader,
    question    = question queryMessage,
    answer      = [],
    authority   = [],
    additional  = []
} where
    queryHeader     = header queryMessage
    queryFlags      = flags queryHeader
    responseFlags   = DNSFlags {
        qOrR            = QR_Response,
        opcode          = opcode queryFlags,
        authAnswer      = False,
        trunCation      = False,
        recDesired      = recDesired queryFlags,
        recAvailable    = False,
        rcode           = rc,
        authenData      = True
    }
    responseHeader  = DNSHeader {
        identifier  = identifier queryHeader,
        flags       = responseFlags
    }

returnError :: ErrorCreator -> DNSMessage -> IO DNSMessage
returnError ec = return . ec

sameIdentifier :: DNSMessage -> DNSMessage -> Bool
sameIdentifier = on (==) $ identifier . header


-- ================================================================================================
-- Configuration
-- ================================================================================================

data DnsServerConfig =
    DnsServerConfig {
        listeningAddress    :: HostName,
        listeningPort       :: ServiceName,
        nameServers         :: NameServers
    }
