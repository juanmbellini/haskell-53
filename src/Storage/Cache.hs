module Storage.Cache 
    ( createInMemoryDnsCacheSystem
    ) where


import Storage.Imports
import Storage.ZoneTree

import Control.Concurrent       (forkIO, threadDelay)
import Control.Monad            (forever)
import Control.Exception.Base   (SomeException, catch)

import Data.IORef
import System.Directory

import Data.List.Split

import Lib


-- | An in memory DNS Cache System, using IORef
data InMemoryDnsCacheSystem =
    InMemoryDnsCacheSystem {
        cache   :: IORef ZoneTree  -- ^ The cache in this system
    }

-- | Make the InMemoryDnsCacheSystem be DNSCacheSystem
instance DnsCacheSystem InMemoryDnsCacheSystem where
    getCache cs dom t = do
        zt <- readIORef $ cache cs
        return $ searchTree zt dom t
    saveCache cs rr = modifyIORef' (cache cs) $ flip addRecord rr


-- | Function that performs all the search mechanism in the tree
searchTree :: ZoneTree -> Domain -> ResourceType -> CacheSearchResult
searchTree zt d t  =   let searchResult = searchWithoutAuthority zt (domainToString d)
                        in case searchResult of
                            InCache []  -> NotAvailable
                            InCache rrs -> onDataAvailable rrs
                            sr          -> sr
    where
        -- | Function that handles the case in which the CacheSearchResult is an InCache.
        --   In this case we must check if there is a ResourceRecord that matches the given ResourceType.
        --   If yes, return it wrapped in a list. If no, apply the onNotMatchingType function.
        onDataAvailable :: [ResourceRecord] -> CacheSearchResult
        onDataAvailable rrs = InCache $ maybe
                                        (onNotMatchingType rrs)         -- If not, then analyze what was returned
                                        (\rr -> [rr])                   -- If yes, then return that RR.
                                        (getMatches resourceType t rrs) -- Does it contains the type we are looking for?

        -- | Function that handles the case in which any result matches the given type.
        --   If there is a CNAME in the given ResourceRecods, then continue search.
        --   Else, return an empty list.
        onNotMatchingType :: [ResourceRecord] -> [ResourceRecord]
        onNotMatchingType = maybe [] searchCNAME . getMatches resourceType CNAME

        -- | Restarts the search process using the name in the given ResourceRecord.
        --   CacheSearchResult is only taken into account if it is InCache.
        searchCNAME :: ResourceRecord -> [ResourceRecord]
        searchCNAME rr  =   let newDom      = getCNAMEData rr
                                recResult   = case searchTree zt newDom t of
                                                InCache rrs -> rrs
                                                _           -> []
                            in rr:recResult
            where
                getCNAMEData :: ResourceRecord -> Domain
                getCNAMEData (ResourceRecord _ _ _ _ (CNAME_Data cannonical))   = cannonical
                getCNAMEData _                                                  = error "Use only with CNAME"


-- | Function that initializes an InMemoryDnsCacheSystem.
createInMemoryDnsCacheSystem :: FilePath -> IO (InMemoryDnsCacheSystem)
createInMemoryDnsCacheSystem fp = do
    zoneTree    <- fmap (maybe emptyTree id) $ retrieveCache fp
    ioRef       <- newIORef zoneTree
    let cacheSystem = InMemoryDnsCacheSystem ioRef
    _ <- forkIO $ persistCacheProcess cacheSystem fp
    return cacheSystem

-- | Tries to retrieve cache data from the given FilePath.
retrieveCache :: FilePath -> IO (Maybe ZoneTree)
retrieveCache fp = do
    catch (fmap (Just . parseZoneTree) $ readFile fp) onErrorHandler
    where
        -- | Error handler to be used when retrieving cache.
        onErrorHandler :: SomeException -> IO (Maybe a)
        onErrorHandler _ = putStrLn "Could not parse ZoneTree" >>= \_ -> return Nothing

        -- | Parses the String into a ZoneTree.
        parseZoneTree :: String -> ZoneTree
        parseZoneTree = read


-- | Starts the persisting cache process.
--   It persists the InMemoryDnsCacheSystem structure in the file with the given FilePath,
--   every minute.
persistCacheProcess :: InMemoryDnsCacheSystem -> FilePath -> IO ()
persistCacheProcess cs fp = do
    createDir fp            -- First, create directory if it not exists
    forever $ do
        persistCache cs fp
        let sleepTime = 60 * 1000000
        threadDelay sleepTime

-- | The action of persisting cache data.
--   It persists the InMemoryDnsCacheSystem structure in the file with the given FilePath.
persistCache :: InMemoryDnsCacheSystem -> FilePath -> IO ()
persistCache cs fp = do
    putStrLn "Persisting cache"
    zt <- readIORef $ cache cs
    let tempFilePath = "/tmp/haskell53/tempCache.txt"
    createDir tempFilePath
    writeFile tempFilePath $ show zt
    copyFile tempFilePath fp


createDir :: FilePath -> IO ()
createDir fp = do
    let parentDirectory = foldl1 (\l s -> l ++ '/':s) . init . splitOn "/" $ fp
    createDirectoryIfMissing True parentDirectory
