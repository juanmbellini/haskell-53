module Main where

import DnsServer

main :: IO ()
main = do
    startDnsServer "0.0.0.0" "7000"
