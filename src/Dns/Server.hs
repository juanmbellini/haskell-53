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
startDnsServer :: (D.DnsCacheSystem c, D.ZonesManager m) => DnsServerConfig -> c -> m -> IO ()
startDnsServer config cacheSystem zonesManager = do
    -- dnsCacheSystem <- initialize cacheSystem
    startServer address port $ handleDns servers cacheSystem zonesManager
    where
        address = listeningAddress config
        port    = listeningPort config
        servers = nameServers config

-- | Handler function for DNSMessages.
handleDns :: (D.DnsCacheSystem c, D.ZonesManager m) => NameServers -> c -> m -> UdpMessage -> IO (Maybe UdpMessage)
handleDns servers cs zm = (=<<) encoder . (=<<) handler . decoder -- Decode binary to DNS, then handle DNS, then encode DNS to binary
    where
        decoder :: UdpMessage -> IO (Maybe DNSMessage)
        decoder = return . decodeUdpMessage
        handler :: Maybe DNSMessage -> IO (Maybe DNSMessage)
        handler = handleDnsRequest servers cs zm
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
handleDnsRequest :: (D.DnsCacheSystem c, D.ZonesManager m) => NameServers -> c -> m -> Maybe DNSMessage -> IO (Maybe DNSMessage)
handleDnsRequest servers cs zm =
    maybe 
        (return Nothing)                                        -- If not present, return IO Nothing
        ((=<<) toIOJust . handleDnsQueryMessage servers cs zm)  -- If present, handle the dns query, then wrap into an IO (Just DNSMessage)


-- | Analyzes the given dns request message, searching for errors.
--   It checks whether the QR flag is the correct (i.e is a query and not a response).
--   It also checks whether the opcode is supported.
--   Finally, it checks whether there are questions.
--   In case any error is detected, the corresponding error creator is returned. 
--   Otherwise, nothing is returned.
analyzeRequest :: DNSMessage -> Maybe ErrorCreator
analyzeRequest msg = if validQR . qOrR $ queryFlags
    then if validOpcde . opcode $ queryFlags
        then if hasQuestion msg
            then Nothing
            else Just createFormatError
        else Just createNotImplementedError
    else Just createFormatError
    where
        queryFlags = flags . header $ msg

        validQR :: QorR -> Bool
        validQR = (==) QR_Query

        validOpcde :: OPCODE -> Bool
        validOpcde = flip elem supportedOpcdes

        hasQuestion :: DNSMessage -> Bool
        hasQuestion = not . null . question


-- | Given the name servers and the cache system, this function handles with them the given DNS message.
--   It analyzes the message, acting according to the result of that analysis.
handleDnsQueryMessage :: (D.DnsCacheSystem c, D.ZonesManager m) => NameServers -> c -> m -> DNSMessage -> IO DNSMessage
handleDnsQueryMessage servers cs zm queryMessage = do
    maybe handleQuery sendError $ analyzeRequest queryMessage
    where
         
        -- | Returns a DNSMessage according on how it is handled the recevied one.
        handleQuery :: IO DNSMessage
        handleQuery = do
            let h               = header queryMessage
                -- Only the first question in the Questions list. More than one question is not supported.
                q               = head $ question queryMessage -- analyzeRequest checked that there are questions
                resolverHandler = resolveQuestion cs servers h q
                cacheHandler    = searchCache cs q >>= analyzeHandlerResult resolverHandler . handleCache h q
            searchManaged zm q >>= analyzeHandlerResult cacheHandler . handleManaged h q
    
        -- | Function that handles the given SearchResult.
        --   Returns error on SearchError, the message on Found, 
        --   and the given IO DNSMessage on NotFound.
        analyzeHandlerResult :: IO DNSMessage -> SearchResult -> IO DNSMessage
        analyzeHandlerResult _               (Found msg) = return msg
        analyzeHandlerResult notFoundHandler NotFound    = notFoundHandler
        analyzeHandlerResult _               SearchError = sendError createFormatError


        -- | Function that takes an error creator and returns a DNSMessage.
        --   It curries the returnError function with the query message (applying a flip function).
        sendError :: ErrorCreator -> IO DNSMessage
        sendError = flip returnError queryMessage


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

-- | A wrapper for search results.
data SearchResult   = Found DNSMessage  -- ^ Data was found.
                    | NotFound          -- ^ Data was not found.
                    | SearchError       -- ^ There are errors in the search.

-- | Searches the Question in the Managed Zones.
searchManaged :: D.ZonesManager m => m -> Question -> IO D.ManagedSearchResult
searchManaged zm q  = maybe
                        (return D.NotManaged)           -- If not, then the desision is to return a NotManaged.
                        (D.getData zm (qname q))        -- If yes, perform the search.
                        (mapToResourceType $ qtype q)   -- Check if we support the question type.

-- | Handles the search result when search in managed data.
handleManaged :: DNSHeader -> Question -> D.ManagedSearchResult -> SearchResult
handleManaged h q (D.Authority rrs) = Found . (authorityResponse h q) . concat . map mapToPackageResourceRecord $ rrs
handleManaged h q (D.Delegated rr)  = Found . (delegatedResponse h q) . mapToPackageResourceRecord $ rr
handleManaged h q (D.NotExists rr)  = Found . (notExistsResponse h q) . mapToPackageResourceRecord $ rr
handleManaged _ _ (D.NotManaged)    = NotFound
handleManaged _ _ (D.QuestionError) = SearchError

-- | Searches the Question in cache.
searchCache :: D.DnsCacheSystem c => c -> Question -> IO D.CacheSearchResult
searchCache cs q = maybe
                    (return D.NotAvailable)         -- If no, then data is not available.
                    (D.getCache cs (qname q))       -- If yes, perform the search.
                    (mapToResourceType $ qtype q)   -- Check if we support the question type.

-- | Handles the search result when search in cache data.
handleCache :: DNSHeader -> Question -> D.CacheSearchResult -> SearchResult
handleCache h q (D.InCache rrs)         = Found . (cacheResponse h q) . concat . map mapToPackageResourceRecord $ rrs
handleCache _ _ (D.NotAvailable)        = NotFound
handleCache _ _ (D.CacheQuestionError)  = SearchError

-- | Resolves the Question externally.
resolveQuestion :: D.DnsCacheSystem c => c -> NameServers -> DNSHeader -> Question -> IO DNSMessage
resolveQuestion cs servers h q = do
    let newReq  = DNSMessage {
            header      = h,    -- Set header
            question    = [q],  -- Send the given question.
            answer      = [],   -- Empty answer.
            authority   = [],   -- Empty authority.
            additional  = []    -- Empty additional.
        }
    result <- forwardRequest servers newReq -- TODO: check forward or recurse
    mapM_ (D.saveCache cs) . mapToOwnResourceRecord $ answer result -- Save all records in cache
    return result

-- | Builds a DNSHeader in response to the query that contained the given DNSHeader,
--   setting the authAnswer and rcode flags according to the given Bool and RCODE values.
buildResponseHeader :: DNSHeader -> Bool -> RCODE -> DNSHeader
buildResponseHeader h auth rc = let msgId   = identifier h
                                    fl      = flags h
                                in DNSHeader {
                                    identifier  = msgId,
                                    flags       = DNSFlags {
                                        qOrR            = QR_Response,
                                        opcode          = opcode fl,
                                        authAnswer      = auth,
                                        trunCation      = False,
                                        recDesired      = recDesired fl,
                                        recAvailable    = False,
                                        rcode           = rc,
                                        authenData      = False
                                    }
                                }

-- | Creates a response DNSMessage to report that data was found in local data
--   (i.e we are authoritative for it).
authorityResponse :: DNSHeader -> Question -> [ResourceRecord] -> DNSMessage
authorityResponse h q rrs = DNSMessage {
                        header      = buildResponseHeader h True NoErr,
                        question    = [q],
                        answer      = rrs,
                        authority   = [],
                        additional  = []
                    }

-- | Creates a response DNSMessage to report that data is delegated
delegatedResponse :: DNSHeader -> Question -> [ResourceRecord] -> DNSMessage
delegatedResponse h q rrs = DNSMessage {
                        header      = buildResponseHeader h False NoErr,
                        question    = [q],
                        answer      = [],
                        authority   = rrs,
                        additional  = []
                    }

-- | Creates a response DNSMessage to report that data is delegated
notExistsResponse :: DNSHeader -> Question -> [ResourceRecord] -> DNSMessage
notExistsResponse h q soaRR = DNSMessage {
                        header      = buildResponseHeader h True NoErr,
                        question    = [q],
                        answer      = [],
                        authority   = soaRR,
                        additional  = []
                    }

-- | Creates a response DNSMessage to report data in cache.
cacheResponse :: DNSHeader -> Question -> [ResourceRecord] -> DNSMessage
cacheResponse h q rrs = DNSMessage {
                        header      = buildResponseHeader h False NoErr,
                        question    = [q],
                        answer      = rrs,
                        authority   = [],
                        additional  = []
                    }

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
