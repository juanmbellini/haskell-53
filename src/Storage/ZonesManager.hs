{-# LANGUAGE OverloadedStrings #-}
module Storage.ZonesManager
    ( createInMemoryZonesManager
    ) where


import Storage.ZoneTree

import Data.IORef

import Storage.Imports

import qualified Data.ByteString.Char8 as C
import Lib


-- | An in memory DNS Cache System, using IORef
data InMemoryZonesManager =
    InMemoryZonesManager {
        zonesData   :: IORef ZoneTree  -- ^ The cache in this system
    }

-- | Make the InMemoryDnsCacheSystem be DNSCacheSystem
instance ZonesManager InMemoryZonesManager where
    -- getData     :: a -> Domain -> ResourceType -> IO (Maybe ResourceRecord)
    getData cs dom t = do
        zt <- readIORef $ zonesData cs
        return $ searchTree zt dom t

-- | Function that performs all the search mechanism in the tree.
searchTree :: ZoneTree -> Domain -> ResourceType -> ManagedSearchResult
searchTree zt d t   =   let searchResult = searchWithAuthority zt (domainToString d)
                        in case searchResult of
                            Authority rrs   -> onAuthority rrs
                            sr              -> sr
    where
        -- | Function that handles the case in which the ManagedSearchResult is an Authority.
        --   In this case we must check if there is a ResourceRecord that matches the given ResourceType.
        --   If yes, return it wrapped in a list. If no, apply the onNotMatchingType function.
        onAuthority :: [ResourceRecord] -> ManagedSearchResult
        onAuthority rrs = Authority $ maybe
                                        (onNotMatchingType rrs)         -- If not, then analyze what was returned
                                        (\rr -> [rr])                   -- If yes, then return that RR.
                                        (getMatches resourceType t rrs) -- Does it contains the type we are looking for?


        -- | Function that handles the case in which any result matches the given type.
        --   If there is a CNAME in the given ResourceRecods, then continue search.
        --   Else, return an empty list.
        onNotMatchingType :: [ResourceRecord] -> [ResourceRecord]
        onNotMatchingType   = maybe [] searchCNAME . getMatches resourceType CNAME

        -- | Restarts the search process using the name in the given ResourceRecord.
        --   ManagedSearchResult is only taken into account if it is Authority.
        searchCNAME :: ResourceRecord -> [ResourceRecord]
        searchCNAME rr  =  let  newDom     = getCNAMEData rr
                                recResult  = case searchTree zt newDom t of
                                                Authority rrs   -> rrs
                                                _               -> []
                            in (rr:recResult)
            where
                getCNAMEData :: ResourceRecord -> Domain
                getCNAMEData (ResourceRecord _ _ _ _ (CNAME_Data cannonical))   = cannonical
                getCNAMEData _                                                  = error "Use only with CNAME"


-- | Function that initializes an InMemoryZonesManager, with the given Zones.
--   The given Zones will be the zones managed by the system 
--   (i.e this server will be authoritative for these Zones).
createInMemoryZonesManager :: [Zone] -> IO (InMemoryZonesManager)
createInMemoryZonesManager = fmap InMemoryZonesManager . newIORef . addZones emptyTree


-- | Creates a new ZoneTree based on the given ZoneTree,
--   adding the given Zones.
addZones :: ZoneTree -> [Zone] -> ZoneTree
addZones = foldl addZone
    where
        -- | Creates a new ZoneTree based on the given ZoneTree,
        --   adding the given Zone.
        addZone :: ZoneTree -> Zone -> ZoneTree
        addZone zt = foldl addRecord zt . fixRecords

        -- | Returns all the ResourceRecords of the given Zone,
        --   applying the buildNewRecord function first to them.
        fixRecords :: Zone -> [ResourceRecord]
        fixRecords (Zone d rs) = map (buildNewRecord d) rs

        -- | Creates a new ResourceRecord, based on the given one,
        --   preppending the given Domain to the ResourceRecord's name.
        buildNewRecord :: Domain -> ResourceRecord -> ResourceRecord
        buildNewRecord d (ResourceRecord n rt rc ttl rdata) =   let newName = case n of
                                                                                ""  -> d
                                                                                _   -> C.append n $ C.append "." d
                                                                in  ResourceRecord newName rt rc ttl rdata
