module Main where


import Dns      (DnsServerConfig(..), startDnsServer)

import Storage  (createInMemoryDnsCacheSystem)


--  | The default configuration.
defaultDnsConf :: DnsServerConfig
defaultDnsConf = DnsServerConfig {
    listeningAddress    = "0.0.0.0",
    listeningPort       = "7000",
    nameServers         = ["8.8.8.8", "8.8.4.4"]
}

-- | Entry point
main :: IO ()
main = do
    cacheSystem <- createInMemoryDnsCacheSystem
    startDnsServer defaultDnsConf cacheSystem
