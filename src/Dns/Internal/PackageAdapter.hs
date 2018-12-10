module Dns.Internal.PackageAdapter
    ( mapToOwnResourceRecord
    , mapToPackageResourceRecord
    , mapToResourceType
    ) where


import Lib
import Dns.Imports
import qualified Dns.Types as D

import Network.DNS.Types hiding     (Domain)


-- | Maps an own ResourceRecord to a ResourceRecord from the Network.DNS.Types package.
mapToPackageResourceRecord :: D.ResourceRecord -> [ResourceRecord]
mapToPackageResourceRecord rr = map createResourceRecord $ mapToRData . D.resourceData $ rr
    where
        createResourceRecord :: RData -> ResourceRecord
        createResourceRecord rd = ResourceRecord {
                                    rrname  = D.name rr,
                                    rrtype  = (mapToTYPE . D.resourceType) rr,
                                    rrclass = (mapToCLASS . D.resourceClass) rr,
                                    rrttl   = D.resourceTtl rr,
                                    rdata   = rd
                                }

-- | Maps a ResourceRecord from the Network.DNS.Types package to an own ResourceRecord.
mapToOwnResourceRecord :: [ResourceRecord] -> [D.ResourceRecord]
mapToOwnResourceRecord rrs = catMaybes . concat . map (\x -> map (\y -> transform y) x) $ grouped
    where
        grouped = map (groupOnNonAdjacent rrtype) $ groupOnNonAdjacent rrname rrs
        transform :: [ResourceRecord] -> Maybe D.ResourceRecord
        transform list   = result
            where
                maybeRT         = mapToResourceType . rrtype . head $ list
                maybeRC         = mapToResourceClass . rrclass . head $ list
                maybeRD         = mapToResourceData . (map rdata) $ list
                validTypeAmount = (==) 1 $ length . nub . map rrtype $ list
                resultData      = Just D.ResourceRecord {
                                    D.name          = rrname . head $ list,
                                    D.resourceType  = fromJust maybeRT,
                                    D.resourceClass = fromJust maybeRC,
                                    D.resourceTtl   = rrttl . head $ list,
                                    D.resourceData  = fromJust maybeRD
                                }
                result          = if validTypeAmount
                                    then if and [isJust maybeRT, isJust maybeRC, isJust maybeRD]
                                        then resultData
                                        else Nothing
                                    else error "The list must have exactly one type"

-- | Maps a TYPE to a Maybe ResourceType.
--   If the given TYPE is not supported, Nothing is returned.
mapToResourceType :: TYPE -> Maybe D.ResourceType
mapToResourceType A     = Just D.A
mapToResourceType NS    = Just D.NS
mapToResourceType CNAME = Just D.CNAME
mapToResourceType SOA   = Just D.SOA
mapToResourceType MX    = Just D.MX
mapToResourceType TXT   = Just D.TXT
mapToResourceType AAAA  = Just D.AAAA
mapToResourceType PTR   = Just D.PTR 
mapToResourceType _     = Nothing  

-- | Maps a ResourceType to a TYPE.
mapToTYPE :: D.ResourceType -> TYPE
mapToTYPE D.A        = A
mapToTYPE D.NS       = NS
mapToTYPE D.CNAME    = CNAME
mapToTYPE D.SOA      = SOA
mapToTYPE D.MX       = MX
mapToTYPE D.TXT      = TXT
mapToTYPE D.AAAA     = AAAA
mapToTYPE D.PTR      = PTR

-- | Maps a CLASS to a ResourceClass
--   If the given CLASS is not supported, Nothing is returned.
mapToResourceClass :: CLASS -> Maybe D.ResourceClass
mapToResourceClass c
                | c == classIN  = Just D.IN
                | otherwise     = Nothing

-- | Maps a ResourceClass to a CLASS
mapToCLASS :: D.ResourceClass -> CLASS
mapToCLASS D.IN = classIN


-- | Maps a list of RData to a ResourceData.
--   Note that if the given list of RData does not contain all elements
--   created with the same constructor, then it will only take into account
--   elements that are "the same" as the first in the list.
--   If the list is empty, or if the first element in the list is not supported, 
--   then Nothing is returned.
mapToResourceData :: [RData] -> Maybe D.ResourceData
mapToResourceData []        = Nothing                       -- If empty, then return Nothing
mapToResourceData (rd:rds)  = maybe
                                Nothing                     -- If not present, then it's not supported.
                                (Just . combine)            -- If present, then it's supported. Combine with the rest.
                                (mapOneToResourceData rd)   -- Map the first element in the list
    where 
        combine = flip (foldl expandResourceData) rds       -- Foldl the rest of the list with the expandResourceData function

        -- | Maps an RData to a Maybe ResourceData.
        --   If the given RData is not supported, nothing is returned.
        mapOneToResourceData :: RData -> Maybe D.ResourceData
        mapOneToResourceData (RD_A a)                               = Just $ D.A_Data [a]
        mapOneToResourceData (RD_NS nss)                            = Just $ D.NS_Data [nss]
        mapOneToResourceData (RD_CNAME ns)                          = Just $ D.CNAME_Data ns
        mapOneToResourceData (RD_SOA mn rn ser refr ret ex mimm)    = Just $ D.SOA_Data mn rn ser refr ret ex mimm
        mapOneToResourceData (RD_MX pref exc)                       = Just $ D.MX_Data [D.MailExchanger exc pref]
        mapOneToResourceData (RD_TXT txt)                           = Just $ D.TXT_Data txt
        mapOneToResourceData (RD_AAAA a)                            = Just $ D.AAAA_Data [a]
        mapOneToResourceData (RD_PTR d)                             = Just $ D.PTR_Data [d]
        mapOneToResourceData _                                      = Nothing
        
        -- | Given an expansible ResourceData, and a RData, if they are compatible, 
        --   then the first one is expanded with data from the second one,
        --   returning the new ResourceData (i.e one with data from both arguments).
        --   In case the given ResourceData is not expansible, or if they are not compatible, 
        --   then the initial ResourceData is returned.
        expandResourceData :: D.ResourceData -> RData -> D.ResourceData
        expandResourceData (D.A_Data as) (RD_A a)               = D.A_Data $ a:as
        expandResourceData (D.NS_Data nss) (RD_NS ns)           = D.NS_Data $ ns:nss
        expandResourceData (D.MX_Data excs) (RD_MX pref exc)    = D.MX_Data $ (D.MailExchanger exc pref):excs
        expandResourceData (D.AAAA_Data as) (RD_AAAA a)         = D.AAAA_Data $ a:as
        expandResourceData (D.PTR_Data ds) (RD_PTR d)           = D.PTR_Data $ d:ds
        expandResourceData r _                                  = r -- Any other case can't be expanded





-- | Maps a ResourceData to a list of RData
--   Note that a list is returned because a single ResourceData can hold more than one RData stuff.
mapToRData :: D.ResourceData -> [RData]
mapToRData (D.A_Data as)                            = map RD_A as
mapToRData (D.NS_Data nss)                          = map RD_NS nss
mapToRData (D.CNAME_Data cn)                        = [RD_CNAME cn]
mapToRData (D.SOA_Data mn rn ser refr ret ex mimm)  = [RD_SOA mn rn ser refr ret ex mimm]
mapToRData (D.MX_Data es)                           = map (\e -> uncurry RD_MX $ mapExchanger e) es
mapToRData (D.TXT_Data t)                           = [RD_TXT t]
mapToRData (D.AAAA_Data as)                         = map RD_AAAA as
mapToRData (D.PTR_Data ds)                          = map RD_PTR ds

-- | Maps a MailExchanger to a (Word16, Domain) tuple (i.e (preference, exchanger fqdn))
mapExchanger :: D.MailExchanger -> (Word16, D.Domain)
mapExchanger e = (pref, exc)
    where
        pref    = D.preference e
        exc     = D.exchange e
