module Dns.Server
    ( DnsServerConfig(..)
    , startDnsServer
    ) where


import Lib
import Udp
import Dns.Imports
import Dns.Internal
import qualified Dns.Types as D

import Network.Socket               (HostName, ServiceName)
import Network.DNS.Decode           (decode)
import Network.DNS.Encode           (encode)
import Network.DNS.Types hiding     (Domain)

import Data.Function                (on)


-- | Type alias for a list of HostName (for better reading when referring to the list of resolver name servers)
type NameServers = [HostName]

-- | Type alias for a function that builds an error response for a given DNSMessage.
type ErrorCreator = DNSMessage -> DNSMessage

-- | Starts a DNSServer with the given configuration and cache system.
startDnsServer :: D.DnsCacheSystem c => DnsServerConfig -> c -> IO ()
startDnsServer config cacheSystem = do
    -- dnsCacheSystem <- initialize cacheSystem
    startServer address port $ handleDns servers cacheSystem
    where
        address = listeningAddress config
        port    = listeningPort config
        servers = nameServers config

-- | Handler function for DNSMessages.
handleDns :: D.DnsCacheSystem c => NameServers -> c -> UdpMessage -> IO (Maybe UdpMessage)
handleDns servers cs = (=<<) encoder . (=<<) handler . decoder -- Decode binary to DNS, then handle DNS, then encode DNS to binary
    where
        decoder :: UdpMessage -> IO (Maybe DNSMessage)
        decoder = return . decodeUdpMessage
        handler :: Maybe DNSMessage -> IO (Maybe DNSMessage)
        handler = handleDnsRequest servers cs
        encoder :: Maybe DNSMessage -> IO (Maybe UdpMessage)
        encoder = return . encodeResponse

-- ================================================================================================
-- Encoding / Decoding
-- ================================================================================================

-- | If present, then encode the given dns message into a udp message.
--   If not present, return Nothing.
encodeResponse :: Maybe DNSMessage -> Maybe UdpMessage
encodeResponse = fmap encode

-- | Tries to decode the given udp message into a dns message.
--   If the given udp message is not a dns message, then Nothing is returned.
decodeUdpMessage :: UdpMessage -> Maybe DNSMessage
decodeUdpMessage = either (\_ -> Nothing) Just . decode


-- ================================================================================================
-- Handling
-- ================================================================================================

-- | List of supported OPCODEs.
supportedOpcdes :: [OPCODE]
supportedOpcdes = [OP_STD]

-- | Given the NameServers and a CacheSystem, if the given Maybe DNSMessage is present,
--   then handle that DNSMessage. If not present, Nothing is returned.
--   Note that the DNSMessage must be a valid query.
handleDnsRequest :: D.DnsCacheSystem c => NameServers -> c -> Maybe DNSMessage -> IO (Maybe DNSMessage)
handleDnsRequest servers cs =
    maybe 
        (return Nothing)                                    -- If not present, return IO Nothing
        ((=<<) toIOJust . handleDnsQueryMessage servers cs) -- If present, handle the dns query, then wrap into an IO (Just DNSMessage)


-- | Analyzes the given dns request message, searching for errors.
--   If checks whether the QR flag is the correct (i.e is a query and not a response).
--   It also checks whether the opcode is supported.
--   In case any error is detected, the corresponding error creator is returned. 
--   Otherwise, nothing is returned.
analyzeRequest :: DNSMessage -> Maybe ErrorCreator
analyzeRequest msg = if validQR . qOrR $ queryFlags
    then if validOpcde . opcode $ queryFlags
        then Nothing
        else Just createNotImplementedError
    else Just createFormatError
    where
        queryFlags = flags . header $ msg

        validQR :: QorR -> Bool
        validQR = (==) QR_Query

        validOpcde :: OPCODE -> Bool
        validOpcde = flip elem supportedOpcdes

-- | Given the name servers and the cache system, this function handles with them the given DNS message.
--   It analyzes the message, acting according to the result of that analysis.
handleDnsQueryMessage :: D.DnsCacheSystem c => NameServers -> c -> DNSMessage -> IO DNSMessage
handleDnsQueryMessage servers cs queryMessage = do
    maybe handleQuery sendError $ analyzeRequest queryMessage
    where
        -- | Function that takes an error creator and returns a DNSMessage.
        --   It curries the returnError function with the query message (applying a flip function).
        sendError :: ErrorCreator -> IO DNSMessage
        sendError = flip returnError queryMessage

        handleQuery :: IO DNSMessage
        handleQuery = do
            -- First check if it is a managed zone
            -- If yes, return the answer according the information 
            -- TODO: missing implementation...
            
            -- If not, then perform the query, trying to answer with cached data first.
            -- First, check cache, returning questions to be performed and answers that are stored.
            (qs, anss) <- checkCache
            -- Log cache results (this is used only to show that cache is working)
            putStrLn $ concat ["There are ", show . length $ anss, " answers in cache"]
            putStrLn $ concat ["Must query externally for ", show . length $ qs, " questions"]
            
            -- Resolves those questions that could not be answered with cached data.
            resolved <- if null qs
                then return ([])
                else queryQuestion qs
            
            -- Stores the resolved questions in cache (if empty, nothing is done).
            saveInCache resolved
            
            -- Return the response with cached and queried answers.
            return . resolvedResponse $ resolved ++ anss

        
        -- | Checks the cache, returning a list of Questions that must be queried (no data in cache for them),
        --   and a list of ResourceRcords that can be answered with data in cache.
        checkCache :: IO ([Question], [ResourceRecord])
        checkCache = do
            res <- mapM questionAndResult $ question queryMessage
            let notInCache = map fst $ filter (\(_, mr) -> isNothing mr) res
                inCache     = concat . map mapToPackageResourceRecord . catMaybes . map snd $ res
            return (notInCache, inCache)
        
        -- | Wraps the given question together with a Maybe ResourceRecord that answers it, taken from cache if it exists
        questionAndResult :: Question -> IO (Question, Maybe D.ResourceRecord)
        questionAndResult q = do
            res <- searchCacheForQuestion q
            return (q, res)
        
        -- | Searches a ResourceRecord that answers the given question.
        --   Nothing is returned if no answers could be found for it.
        searchCacheForQuestion :: Question -> IO (Maybe D.ResourceRecord)
        searchCacheForQuestion q = maybe
                                    (return Nothing)                -- If not, then return Nothing
                                    doSearchCache       -- If yes, then look in cache
                                    -- (D.getCache cs $ qname q)       -- If yes, then look in cache
                                    (mapToResourceType $ qtype q)   -- Check if we support the question type
                                        -- TODO: when empty, search for a CNAME instead
            where
                doSearchCache :: D.ResourceType -> IO (Maybe D.ResourceRecord)
                doSearchCache t = do
                    partial <- D.getCache cs (qname q) t
                    if isJust partial
                        then return partial
                        else case t of
                                D.CNAME -> return Nothing
                                _       -> return Nothing
                                -- TODO: here we must continue searching in CNAMEs till
                                -- TODO: a record of type t is returned. If another is returned, return nothing

        
        -- | Saves in cache the given ResourceRecords.
        saveInCache :: [ResourceRecord] -> IO ()
        saveInCache = mapM_ (D.saveCache cs) . mapToOwnResourceRecord

        -- | Creates a response DNSMessage, sending the given ResourceRecords in the answers section.
        resolvedResponse :: [ResourceRecord] -> DNSMessage
        resolvedResponse rrs = DNSMessage {
                                header      = respHeader,
                                question    = question queryMessage,
                                answer      = rrs,
                                authority   = [],
                                additional  = []
                            }
            where
                queryHeader     = header queryMessage
                queryFlags      = flags queryHeader
                responseFlags   = DNSFlags {
                                    qOrR            = QR_Response,
                                    opcode          = opcode queryFlags,
                                    authAnswer      = False,
                                    trunCation      = False,
                                    recDesired      = recDesired queryFlags,
                                    recAvailable    = False,
                                    rcode           = NoErr,
                                    authenData      = True
                                }
                respHeader      = DNSHeader {
                                    identifier  = identifier queryHeader,
                                    flags       = responseFlags
                                }
                        

        -- | Resolves the given questions.
        queryQuestion :: [Question] -> IO [ResourceRecord]
        queryQuestion qs = do
            let newReq  = DNSMessage {
                    header      = header queryMessage,      -- Copy headers from the query message.
                    question    = qs,                       -- Send the given questions.
                    answer      = answer queryMessage,      -- Copy answers (should be empty though).
                    authority   = authority queryMessage,   -- Copy authority (should be empty though).
                    additional  = additional queryMessage   -- Copy additional (should be empty though).
                }
            fmap answer $ forwardRequest servers newReq     -- TODO: check forward or recurse

-- | Forwards the the given DNSMessage to the given NameServers, returning the DNSMessage response.
--   In case there is no response, a server error is returned.
forwardRequest :: NameServers -> DNSMessage  -> IO DNSMessage
forwardRequest servers dnsReq = do
    -- TODO: retry? use another name server? Also, catch "Network.Socket.recvBuf: does not exist (Connection refused)"
    resp <- sendAndReceive (head servers) "53" 500000 $ encode dnsReq
    maybe
        sendError           -- In case of not getting a response from nameservers, return server error
        handleUdpMessage    -- If response was received, handle it.
        resp
    where
        -- | The DNSMessage with the server error to be sent 
        --   in case the received DNSMessage does not pass the identifier verification.
        sendError :: IO DNSMessage
        sendError = returnError createServerError dnsReq            -- Create the server error from the query

        -- | Verifies the given DNSMessage, testing if it has the same identifier as the sent one.
        identifierVerification :: DNSMessage -> IO DNSMessage
        identifierVerification resp = do
            if sameIdentifier dnsReq resp   -- Check if the identifier is the same
                then return resp            -- Return response if yes
                else sendError              -- Return error if not

        -- | Handles the received DNSMessage. If Nothing is passed, a ServerError is returned. 
        handleDnsMessage :: Maybe DNSMessage -> IO DNSMessage
        handleDnsMessage = maybe sendError identifierVerification   -- If DNSMessage, then perform identifier verification. Else send error

        -- | Handles the received UPDMessage.
        handleUdpMessage :: UdpMessage -> IO DNSMessage
        handleUdpMessage = (=<<) handleDnsMessage . (return . decodeUdpMessage)  -- Decode the UdpMessage and try to handle it as DNS



-- ================================================================================================
-- Helpers
-- ================================================================================================

-- | Wrapper for an ErrorCreator with the FormatErr RCODE.
createFormatError :: ErrorCreator
createFormatError = createError FormatErr

-- | Wrapper for an ErrorCreator with the ServFail RCODE.
createServerError :: ErrorCreator
createServerError = createError ServFail

-- | Wrapper for an ErrorCreator with the NotImpl RCODE.
createNotImplementedError :: ErrorCreator
createNotImplementedError = createError NotImpl

-- | Function that creates an ErrorCreator function given an RCODE (i.e the error to be returned).
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

-- | Function that returns an IO DNSMessage by composing the given ErrorCreator with a return function.
--   The given DNSMessage is used by the ErrorCreator to take data from it to build the error response.
returnError :: ErrorCreator -> DNSMessage -> IO DNSMessage
returnError ec = return . ec

-- | Function that tests if two DNSMessage have the same identifier.
sameIdentifier :: DNSMessage -> DNSMessage -> Bool
sameIdentifier = on (==) $ identifier . header

-- ================================================================================================
-- Configuration
-- ================================================================================================

data DnsServerConfig =
    DnsServerConfig {
        listeningAddress    :: HostName,    -- ^ Address to which the server must bind.
        listeningPort       :: ServiceName, -- ^ Listing port for the server.
        nameServers         :: NameServers, -- ^ Nameservers to which the request can be forwarded.
        rootServers         :: [IPv4],      -- ^ The addresses of the root servers.
        zones               :: [D.Zone]     -- ^ The managed zones.
    } deriving Show
