module Main where

import UdpServer

main :: IO ()
main = do
    startServer "0.0.0.0" "7000" (\req -> do return (Just req))
