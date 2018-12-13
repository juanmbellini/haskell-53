module Storage.Cache 
    ( createInMemoryDnsCacheSystem
    ) where


import Storage.Imports
import Storage.ZoneTree

import Data.IORef


-- | An in memory DNS Cache System, using IORef
data InMemoryDnsCacheSystem =
    InMemoryDnsCacheSystem {
        cache   :: IORef ZoneTree  -- ^ The cache in this system
    }

-- | Make the InMemoryDnsCacheSystem be DNSCacheSystem
instance DnsCacheSystem InMemoryDnsCacheSystem where
    getCache cs dom t = do
        zt <- readIORef $ cache cs
        return $ searchZone zt (domainToString dom) t -- TODO: invalidate if TTL is below zero
    saveCache cs rr = modifyIORef' (cache cs) $ flip addRecord rr

-- | Function that initializes an InMemoryDnsCacheSystem.
createInMemoryDnsCacheSystem :: IO (InMemoryDnsCacheSystem)
createInMemoryDnsCacheSystem = newIORef emptyTree >>= return . InMemoryDnsCacheSystem
