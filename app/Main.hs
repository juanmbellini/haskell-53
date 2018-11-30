module Main where

import DnsServer


defaultDnsConf :: DnsServerConfig
defaultDnsConf = DnsServerConfig {
    listeningAddress    = "0.0.0.0",
    listeningPort       = "7000",
    nameServers         = ["8.8.8.8", "8.8.4.4"]
}

main :: IO ()
main = do
    startDnsServer defaultDnsConf
