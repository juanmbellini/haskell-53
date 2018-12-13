module Config.SystemConfig
    ( SystemConfig(..)
    ) where


import Dns (DnsServerConfig)


-- | The system configuration.
data SystemConfig =
    SystemConfig {
        dnsConfig           :: DnsServerConfig, -- ^ Configuration stuff for the DNS server
        cacheDataFilePath   :: FilePath         -- ^ File path for the persisted cached data (for reading and writing)
    } deriving Show
