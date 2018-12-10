module Dns.Types where


import Dns.Imports


-- ================================================================================================
-- DNS Stuff
-- ================================================================================================

-- | The domain type
type Domain         = ByteString        -- A domain name is a string

-- | Resource records types
data ResourceType   = A                 -- ^ IPv4 Address
                    | NS                -- ^ An authoritative name server
                    | CNAME             -- ^ The canonical name for an alias
                    | SOA               -- ^ Marks the start of a zone of authority
                    | MX                -- ^ Mail exchange record
                    | TXT               -- ^ Text string record
                    | AAAA              -- ^ IPv6 Address
                    | PTR               -- ^ A domain name pointer
                    deriving (Eq, Ord, Show)

-- | The resource record class
data ResourceClass  = IN                -- Only Internet is supported
    deriving (Eq, Show)

-- | The TTL type
type ResourceTTL    = Word32            -- Represented as an unsigned 32-bits integer

-- | Data for each type of record
data ResourceData   = A_Data {
                        addresses   :: [IPv4]           -- ^ IPv4 Addresses
                    } 
                    | NS_Data {
                        nServers    :: [Domain]         -- ^ The domain names of authoritative name servers
                    }
                    | CNAME_Data {
                        cname       :: Domain           -- ^ The canonical name for an alias
                    }
                    | SOA_Data {
                        mname       :: Domain,          -- ^ The domain name of the primary server for this zone
                        rname       :: Domain,          -- ^ The domain name that specifies the mailbox of the person responsible for this zone
                        serial      :: Word32,          -- ^ A number indicating the version of this zone (used for data synchronization)
                        refresh     :: Word32,          -- ^ Amount of time in seconds that must elapse before refreshing data
                        retry       :: Word32,          -- ^ Amount of time in seconds that must elapse before retrying a failed refresh
                        expire      :: Word32,          -- ^ Amounf of time in seconds that must elapse before losing the authoritative privilege
                        minimum     :: Word32           -- ^ The minimum TTL that should be exported with any RR from this zone
                    }
                    | MX_Data {
                        exchangers  :: [MailExchanger]  -- ^ The exchangers
                    }
                    | TXT_Data {
                        txtData     :: ByteString       -- ^ A text string.
                    }
                    | AAAA_Data {
                        addresses6  :: [IPv6]           -- ^ IPv6 Addresses.
                    }
                    | PTR_Data {
                        ptrdname    :: [Domain]         -- ^ A domain name that point to the same location in the domain name space.
                    }
                    deriving (Eq, Show)

-- | Data type for a Mail Exchanger, indicating its domain and preference
data MailExchanger =
    MailExchanger {
        exchange    :: Domain,  -- ^ The domain name of a host that acts as a mail exchanger
        preference  :: Word16   -- ^ Specifies the preference for this exchanger. Lower values are prefered
    }
    deriving (Eq, Show)

-- | A Resource record
data ResourceRecord =
    ResourceRecord {
        name            :: Domain,          -- ^ The name for the resource (i.e the complete fqdn)
        resourceType    :: ResourceType,    -- ^ The resource record type
        resourceClass   :: ResourceClass,   -- ^ The resource record class
        resourceTtl     :: ResourceTTL,     -- ^ The TTL for the record
        resourceData    :: ResourceData     -- ^ The resource's data
    } 
    deriving (Eq, Show)

-- | A zone data type
data Zone =
    Zone {
        domain  :: Domain,          -- The zone domain name
        records :: [ResourceRecord] -- A list of resource records
    }
    deriving Eq


-- ================================================================================================
-- Cache Stuff
-- ================================================================================================

-- | A type class for types that can behave as DNS Cache Systems.
--   A DNS Cache System must be able to be queried for, and store, DNS data.
class DnsCacheSystem a where
    -- | Given a DNSCacheSystem, a Domain, and a ResourceType, 
    --   if data is avaiable, return Just it; otherwise, Nothing is returned.
    getCache    :: a -> Domain -> ResourceType -> IO (Maybe ResourceRecord)
    
    -- | Given a DNSCacheSystem, it stores the given ResourceRecord.
    saveCache   :: a -> ResourceRecord -> IO ()
