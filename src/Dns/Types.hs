module Dns.Types
    ( Domain
    , ResourceType(..)
    , ResourceClass(..)
    , ResourceTTL
    , ResourceData(..)
    , MailExchanger(..)
    , ResourceRecord(..)
    , Zone(..)
    , domainToString
    , stringToDomain
    , ManagedSearchResult(..)
    , CacheSearchResult(..)
    , ZonesManager(..)
    , DnsCacheSystem(..)
    ) where


import Dns.Imports

import Data.ByteString.Char8 (pack, unpack)


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
    deriving (Eq, Show)

-- | Transforms the given Domain (which is a ByteString alias) into a String
domainToString :: Domain -> String
domainToString = unpack

-- | Transforms the given String into a Domain (which is a ByteString alias).
stringToDomain :: String -> Domain
stringToDomain = pack

-- ================================================================================================
-- Searching Stuff
-- ================================================================================================

-- | Data type for the possible return values of the search algorithm.
data ManagedSearchResult    = Authority { authRecords   :: [ResourceRecord] }   -- ^ The whole domain is matched.               (OPCODE 0)
                            | Delegated { nsRecord      :: ResourceRecord   }   -- ^ The nearest Zone contains an NS record.    (OPCODE 0)
                            | QuestionError                                     -- ^ The queried domain is not well formatted.  (OPCODE 1)
                            | NotExists                                         -- ^ No match with the nearest Zone.            (OPCODE 3)
                            | NotManaged                                        -- ^ No nearest ancestor Zone.                  (OPCODE 5)

-- | A type class for types that can behave as a Zones Manager.
--   It must be able to retrieve DNS records.
class ZonesManager a where
    -- | Given a ZoneManager, a Domain, and a ResourceType, 
    --   if data is avaiable, return Just it; otherwise, Nothing is returned.
    getData     :: a -> Domain -> ResourceType -> IO ManagedSearchResult


-- | Data type for the possible return values of the search algorithm.
data CacheSearchResult      = InCache   { inCacheData   :: [ResourceRecord] }   -- ^ Data is available in cache.
                            | NotAvailable                                      -- ^ Data is not available in cache.
                            | CacheQuestionError                                -- ^ The queried domain is not well formatted.
                            deriving Show

-- | A type class for types that can behave as a DNS Cache Systems.
--   A DNS Cache System must be able to be queried for, and store, DNS data.
class DnsCacheSystem a where
    -- | Given a DNSCacheSystem, a Domain, and a ResourceType, 
    --   if data is avaiable, return Just it; otherwise, Nothing is returned.
    getCache    :: a -> Domain -> ResourceType -> IO CacheSearchResult
    
    -- | Given a DNSCacheSystem, it stores the given ResourceRecord.
    saveCache   :: a -> ResourceRecord -> IO ()
