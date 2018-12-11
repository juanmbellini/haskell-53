module Storage.ZoneTree
    ( ZoneTree(..)
    , emptyTree
    , searchZone
    , addRecord
    ) where


import Storage.Imports

import Data.List
import Data.List.Split
import Control.Monad


-- | A Zone tree. This data structure is used to store zones data, and query it efficiently.
data ZoneTree = 
    ZoneTree {
        label       :: String,              -- ^ The label for this zone
        zoneData    :: [ResourceRecord],    -- ^ The data in this zone node
        children    :: [ZoneTree]           -- ^ The children nodes (i.e children zones)
    }
    deriving Eq

-- | An empty zones tree (i.e to be used for initialization)
emptyTree :: ZoneTree
emptyTree = ZoneTree "" [] []


-- ================================================================================================
-- Search
-- ================================================================================================

-- | Searches for a domain of the given type in the given zone tree.
--   The result is wrapped in a Maybe, returning Nothing if no match, or Just the result.
searchZone :: ZoneTree -> String -> ResourceType -> Maybe ResourceRecord
searchZone zt dom t = join . fmap (flip (searchZoneRec t) zt) $ splitDomain dom

-- | Recursive function that searches in the given zone tree.
--   The function is called recursively going forward with the next label and child (if they match).
--   When there are no more labels, it searches the data in the actual node.
searchZoneRec :: ResourceType -> [String] -> ZoneTree -> Maybe ResourceRecord
searchZoneRec _ (_:_)   (ZoneTree _ _ [])   = Nothing                                                   -- Labels list still has elements but there are no more children nodes.
searchZoneRec t []      (ZoneTree _ rrs _)  = getMatches rrs resourceType t                             -- We are in the node that might contain the data we are searching.
searchZoneRec t (l:ls)  (ZoneTree _ _ cs)   = join . fmap (searchZoneRec t ls) $ getMatches cs label l  -- There are labels in the list, so search recursively.


-- ================================================================================================
-- Insert
-- ================================================================================================

-- | Adds a record to a zone tree. 
--   If a record exists with the same domain and type, then it is replaced.
addRecord :: ZoneTree -> ResourceRecord -> ZoneTree
addRecord zt rr = maybe
                        (error "The given domain is not valid") -- If it's Nothing, the domain is invalid.
                        (flip (addRecordRec rr) zt)             -- Else, call the addRecordRec function.
                        (splitDomain . domainToString . name $ rr)    -- Check whether the splitted labels contain data.

-- | Recursive function that takes a record, a list of labels and a zone tree,
--   and returns another zone tree with the record added into it.
--   The record will be added in the node in which the labels list is consumed.
addRecordRec :: ResourceRecord -> [String] -> ZoneTree -> ZoneTree
addRecordRec rr [] (ZoneTree lab rrs cs) = ZoneTree lab newRrs cs       -- In this case we are in the node where data must be stored
    where
        newRrs      = rr:preparedRrs
        preparedRrs = maybe
                        rrs                 -- If no record with the given type, return as is.
                        (flip delete rrs)   -- If there is a record with the given type, delete.
                        record              -- Check if there are resources of the given type.
        record      = getMatches rrs resourceType $ resourceType rr
addRecordRec rr (l:ls) (ZoneTree lab rrs cs) = ZoneTree lab rrs newCs   -- In this case, we must continue traversing the tree.
    where
        newCs           = newChild:preparedCs
        newChild        = addRecordRec rr ls c          -- Add recursively.
        (c, preparedCs) = maybe                         
                            (ZoneTree l [] [], cs)      -- If no child, create a new one and return the children list as is.
                            (\x -> (x, delete x cs))    -- If child exists, return it together with the list without it.
                            (getMatches cs label l)     -- Get the child that matches


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

-- | Get matches in the given list, mapping the elements with the given function.
--   A match is considered to be found when the mapping is equal to the given value.
--   When matches are found, then the first element in the matching list is returned.
getMatches :: Eq b => [a] -> (a -> b) -> b -> Maybe a
getMatches list f v = if null matches           -- Check if there are elements that matched.
    then Nothing                                -- If there weren't then return Nothing.
    else Just $ head matches                    -- Else, get the first element (which should be the only one), and wrap in Just.
    where
        matches = filter ((==) v . f) list    -- Filter for elements that map to the given value (should be only one).
