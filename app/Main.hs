module Main where


import Dns      (startDnsServer, DnsServerConfig(..), domain)
import Storage  (createInMemoryDnsCacheSystem, createInMemoryZonesManager)
import Config   (getConfiguration, SystemConfig(..))

import Data.IP


-- | Default config file
--   TODO: SHOULD WE KEEP THIS?
configFilePath :: FilePath
configFilePath = "/Users/jbellini/Desktop/exampleConfigFile.json"

-- | Reports the given configuration
--   (used for giving INFO logging about how the system is operating).
reportConfigration :: SystemConfig -> IO ()
reportConfigration conf = do
    putStrLn "Operating with the following configuration:"
    let dnsConf = dnsConfig conf
    putStrLn . (++) "\tAddress:\t\t"                $ listeningAddress dnsConf
    putStrLn . (++) "\tPort:\t\t\t"                 $ listeningPort dnsConf
    putStrLn . (++) "\tForwarding requests to:\t"   $ separateElements . show . map stringToIPv4 . nameServers $ dnsConf
    putStrLn . (++) "\tManaged zones:\t\t"          $ separateElements . show . map domain . zones $ dnsConf
    putStrLn . (++) "\tSaving cache on:\t"          $ show . cacheDataFilePath $ conf

-- | Takes a String and tries to parse it into an IPv4.
stringToIPv4 :: String -> IPv4
stringToIPv4 = read

-- | Takes a String and replaces each ',' occurrance by a ", ".
--   This functions is meant to be used by lists' String representations.
separateElements :: String -> String
separateElements []         = []
separateElements (',':cs)   = ',':' ':separateElements cs
separateElements (c:cs)     = c:separateElements cs


-- | Entry point
main :: IO ()
main = do
    systemConfig <- getConfiguration configFilePath
    reportConfigration systemConfig
    let dnsConf     = dnsConfig systemConfig
    let cacheFIle   = cacheDataFilePath systemConfig
    cacheSystem <- createInMemoryDnsCacheSystem cacheFIle
    zoneManager <- createInMemoryZonesManager (zones dnsConf)

    startDnsServer dnsConf cacheSystem zoneManager
