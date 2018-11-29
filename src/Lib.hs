module Lib
    ( toIOJust
    ) where

toIOJust :: a -> IO (Maybe a)
toIOJust = return . Just
