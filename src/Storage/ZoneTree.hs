module Storage.ZoneTree
    ( ZoneTree(..)
    , ManagedSearchResult(..)
    , CacheSearchResult(..)
    , emptyTree
    , searchWithoutAuthority
    , searchWithAuthority
    , addRecord
    ) where


import Storage.Imports

import Data.List
import Data.List.Split

import Lib


-- | A Zone tree. This data structure is used to store zones data, and query it efficiently.
data ZoneTree = 
    ZoneTree {
        label       :: String,              -- ^ The label for this zone
        zoneData    :: [ResourceRecord],    -- ^ The data in this zone node
        children    :: [ZoneTree]           -- ^ The children nodes (i.e children zones)
    }
    deriving (Eq, Show, Read)

-- | An empty zones tree (i.e to be used for initialization)
emptyTree :: ZoneTree
emptyTree = ZoneTree "" [] []


-- ================================================================================================
-- Search
-- ================================================================================================

-- ================================================
-- Non Authority stuf
-- ================================================

-- -- | Searches for a domain of the given type in the given zone tree.
-- --   The result is wrapped in a Maybe, returning Nothing if no match, or Just the result.
-- searchZone :: ZoneTree -> String -> ResourceType -> Maybe ResourceRecord
-- searchZone zt dom t = join . fmap (flip (searchZoneRec t) zt) . splitDomain . toUpperCase $ dom

-- -- | Recursive function that searches in the given zone tree.
-- --   The function is called recursively going forward with the next label and child (if they match).
-- --   When there are no more labels, it searches the data in the actual node.
-- searchZoneRec :: ResourceType -> [String] -> ZoneTree -> Maybe ResourceRecord
-- searchZoneRec _ (_:_)   (ZoneTree _ _ [])   = Nothing                                                       -- Labels list still has elements but there are no more children nodes.
-- searchZoneRec t []      (ZoneTree _ rrs _)  = getMatches resourceType t rrs                                -- We are in the node that might contain the data we are searching.
-- searchZoneRec t (l:ls)  (ZoneTree _ _ cs)   = join . fmap (searchZoneRec t ls) $ getMatches label l cs     -- There are labels in the list, so search recursively.


-- | Searches in the given tree for data about the given domain.
--   Authority is not taken into account to respond.
--   This function is to be used for searching cache.
searchWithoutAuthority :: ZoneTree -> String -> CacheSearchResult
searchWithoutAuthority zt dom = maybe
                                CacheQuestionError
                                (searchWithoutAuthorityRec zt)
                                (splitDomain . toUpperCase $ dom)

-- | Searches in the given tree node for a child with the label
--   that matches the head of the given labels list.
--   Authority is not taken into account to respond.
--   This function is to be used for searching cache.
searchWithoutAuthorityRec :: ZoneTree -> [String] -> CacheSearchResult
searchWithoutAuthorityRec (ZoneTree _ _ [])     (_:_)   = NotAvailable      -- Labels list still has elements but there are no more children nodes.
searchWithoutAuthorityRec (ZoneTree _ rrs _)    []      = InCache rrs       -- No more labels, so we are in an exact match. Return whatever data is present.
searchWithoutAuthorityRec (ZoneTree _ _ cs)     (l:ls)  = searchChildren    -- There are labels in the list, so check children.
    where
        searchChildren  = maybe NotAvailable                        -- If not present, data is not available.
                            (flip searchWithoutAuthorityRec ls)     -- If present, search recursively i
                            (getMatches label l cs)                 -- Get child that matches label



-- ================================================
-- Authority stuff
-- ================================================

-- | A wrapper structure for a SearchResult,
--   indicating if the wrapped data has authority confirmed.
data Authoritative  = Confirmed    ManagedSearchResult
                    | NotConfirmed ManagedSearchResult

-- | Searches in the given tree data about the given domain.
--   Authority is taken into account to respond.
--   This function is to be used for searching managed zones.
searchWithAuthority :: ZoneTree -> String -> ManagedSearchResult
searchWithAuthority zt dom = maybe
                                QuestionError
                                (analyzeResult . searchWithAuthorityRec zt)
                                (splitDomain . toUpperCase $ dom)
    where
        -- | Analyzes the given Authoritative result.
        --   If not confirmed, then we now is not a managed zone.
        --   If confirmed, then return wrapped result.
        analyzeResult :: Authoritative -> ManagedSearchResult
        analyzeResult   (Confirmed sr)      = sr
        analyzeResult   (NotConfirmed _)    = NotManaged




-- | Searches in the given tree node for a child with the label
--   that matches the head of the given labels list.
--   Authority is taken into account to respond.
--   This function is to be used for searching managed zones.
searchWithAuthorityRec :: ZoneTree -> [String] -> Authoritative

-- | Exact match.
searchWithAuthorityRec  (ZoneTree _ rrs _)  []  = if hasSoa rrs                     -- Check if it has SOA
                                                    then Confirmed nodeData         -- If yes, return data as is.
                                                    else NotConfirmed delegation    -- Else, if not, check delegation.
    where
        nodeData    = Authority rrs                 -- The node data (i.e is authoritative for it).
        delegation  = getDelegation nodeData rrs    -- Check if an NS record is present, returning the node data if not.

-- | There are still labels to be matched.
searchWithAuthorityRec  (ZoneTree _ rrs cs) (l:ls)  = maybe noResult searchChild matchedChild
    where

        -- | When there is no result, then if actual node is SOA, then we can confirm that record does not exists.
        --   Else, if not SOA, then we must check if an NS record is contained.
        --   If NS record exists, then we are delegating. If not, the domain is not managed.
        noResult :: Authoritative
        noResult = getSoaRecord (NotConfirmed $ getDelegation NotManaged rrs) (Confirmed . NotExists) rrs

        -- | Searches recursively in the given ZoneTree.
        --   If the result is has authoritiy confirmed, then return as is
        --   Else, if not, we must analyze the result, together with context data.
        searchChild :: ZoneTree -> Authoritative
        searchChild child   = case searchWithAuthorityRec child ls of
                                Confirmed      sr               -> Confirmed sr
                                NotConfirmed   (NotExists _)    -> error "Received non authoritative NotExists."
                                NotConfirmed   NotManaged       -> noResult
                                NotConfirmed   sr               -> (if hasSoa rrs then Confirmed else NotConfirmed) sr

        -- | Searches for the child that matches it's label with the actual label.
        --   If no child matched, Nothing is returned. Else, Just the child is returned.
        matchedChild :: Maybe ZoneTree
        matchedChild = getMatches label l cs




-- ================================================================================================
-- Insert
-- ================================================================================================

-- | Adds a record to a zone tree. 
--   If a record exists with the same domain and type, then it is replaced.
addRecord :: ZoneTree -> ResourceRecord -> ZoneTree
addRecord zt rr =   maybe
                        (error ("The given domain is not valid:" ++ upperCaseName)) -- If it's Nothing, the domain is invalid.
                        (flip (addRecordRec rr) zt)             -- Else, call the addRecordRec function.
                        (splitDomain upperCaseName)             -- Check whether the splitted labels contain data.
    where
        upperCaseName :: String
        upperCaseName = toUpperCase . domainToString . name $ rr
        -- upperCaseRR :: ResourceRecord
        -- upperCaseRR = ResourceRecord {
        --     name            = stringToDomain upperCaseName, -- Domains must be stored in UPPERCASE
        --     name            = stringToDomain upperCaseName, -- Domains must be stored in UPPERCASE
        --     resourceType    = resourceType rr,
        --     resourceClass   = resourceClass rr,
        --     resourceTtl     = resourceTtl rr,
        --     resourceData    = resourceData rr
        -- }

-- | Recursive function that takes a record, a list of labels and a zone tree,
--   and returns another zone tree with the record added into it.
--   The record will be added in the node in which the labels list is consumed.
addRecordRec :: ResourceRecord -> [String] -> ZoneTree -> ZoneTree
addRecordRec rr [] (ZoneTree lab rrs cs) = ZoneTree lab newRrs cs       -- In this case we are in the node where data must be stored
    where
        -- upperCaseLabel  = toU
        newRrs      = rr:preparedRrs
        preparedRrs = maybe
                        rrs                 -- If no record with the given type, return as is.
                        (flip delete rrs)   -- If there is a record with the given type, delete.
                        record              -- Check if there are resources of the given type.
        rtype       = resourceType rr
        record      = getMatches resourceType rtype rrs
addRecordRec rr (l:ls) (ZoneTree lab rrs cs) = ZoneTree lab rrs newCs   -- In this case, we must continue traversing the tree.
    where
        newCs           = newChild:preparedCs
        newChild        = addRecordRec rr ls c          -- Add recursively.
        (c, preparedCs) = maybe                         
                            (ZoneTree l [] [], cs)      -- If no child, create a new one and return the children list as is.
                            (\x -> (x, delete x cs))    -- If child exists, return it together with the list without it.
                            (getMatches label l cs)     -- Get the child that matches


-- ================================================================================================
-- Helpers
-- ================================================================================================

-- | Splits the given domain string in a list of labels that make up that domain.
--   If the root domain (i.e the "." domain) is given, an empty list is returned.
splitDomain :: String -> Maybe [String]
splitDomain dom = case dom of
    "." -> Just []                              -- In this case, we must search in the root servers
    _   -> verifyLabels . splitInLabels $ dom   -- In this case, we must split into labels separated by ".".
    where
        verifyLabels :: [String] -> Maybe [String]
        verifyLabels ls = if elem "" ls -- If there is an empty label, then the request is malformed.
            then Nothing                -- So, in that case, we return Nothing.
            else Just ls                -- Else, the request is wellformed, so we wrap it in a Just.
        
        splitInLabels :: String -> [String]
        splitInLabels "."   = error "Do not use with root domain"   -- If the domain is the root servers, then this function should not be used
        splitInLabels x     = reverse . endBy "." $ x

-- | Returns Delegated Search Result with delegation data
--   if it exists in the given list of ResourceRecords.
--   If there is no NS data then return the given default SearchResult.
getDelegation :: ManagedSearchResult -> [ResourceRecord] -> ManagedSearchResult
getDelegation sr = maybe sr Delegated . getMatches resourceType NS

getSoaRecord :: a -> (ResourceRecord -> a) -> [ResourceRecord] -> a
getSoaRecord v f rrs = maybe v f (getMatches resourceType SOA rrs)

-- | Function that takes a list of ResourceRecord,
--   and returns True if it contains a SOA record, or False otherwise.
hasSoa :: [ResourceRecord] -> Bool
hasSoa = getSoaRecord False (\_ -> True)
