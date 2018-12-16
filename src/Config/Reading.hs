module Config.Reading
    ( getConfiguration
    ) where


import Dns                  (DnsServerConfig(..))
import Config.SystemConfig  (SystemConfig(..))
import Config.Internal.Json

import Data.Maybe
import Control.Exception.Base   (SomeException, catch)

import qualified Data.ByteString.Lazy.Char8 as L (readFile)


-- ================================================================================================
-- Parsing
-- ================================================================================================

getConfiguration :: FilePath -> IO (SystemConfig)
-- getConfiguration = (=<<) (return . fromMaybe defaultConfig . parseSystemConfig) . C.readFile
getConfiguration fp = do
    putStrLn ("Parsing file " ++ fp)
    -- conf <- fmap parseSystemConfig (L.readFile fp)
    conf <- catch (fmap parseSystemConfig $ L.readFile fp) onErrorHandler
    let message = case conf of
                    Nothing -> "Could not parse config file. Using default."
                    Just _  -> "Parsed config file successfully."
    putStrLn message
    return . fromMaybe defaultConfig $ conf
    where
        -- | Error handler to be used when retrieving cache.
        onErrorHandler :: SomeException -> IO (Maybe a)
        onErrorHandler _ = putStrLn "Could not config file" >>= \_ -> return Nothing

-- ================================================================================================
-- Default Configuration
-- ================================================================================================

-- | Default System Configuration
defaultConfig :: SystemConfig
defaultConfig =
    SystemConfig {
        cacheDataFilePath   = "/tmp/haskell53/cache.txt",
        dnsConfig           = defaultDnsConf
    }

--  | The default DNS configuration.
defaultDnsConf :: DnsServerConfig
defaultDnsConf = DnsServerConfig {
    listeningAddress    = "0.0.0.0",
    listeningPort       = "7000",
    nameServers         = ["8.8.8.8", "8.8.4.4"],
    rootServers         = map read [
                            "198.41.0.4",
                            "199.9.14.201",
                            "192.33.4.12",
                            "199.7.91.13",
                            "192.203.230.10",
                            "192.5.5.241",
                            "192.112.36.4",
                            "198.97.190.53",
                            "192.36.148.17",
                            "192.58.128.30",
                            "193.0.14.129",
                            "199.7.83.42",
                            "202.12.27.33"
                        ],
    zones               = []
}
