module Lib
    ( toIOJust
    , groupOnNonAdjacent
    , toUpperCase
    , getMatches
    ) where


import Data.List
import Data.Function    (on)
import Data.Char


-- | Wraps the given element into an IO . Just
toIOJust :: a -> IO (Maybe a)
toIOJust = return . Just

-- | Groups the given elements of the list, according to the given function.
--   Note that the result of the given function must be an Ordered type 
--   (we must sort the list with this function to group).
--   The result is a list of lists, where each element in the internal list
--   maps to the same result when applying the given mapping function.
groupOnNonAdjacent :: Ord b => (a -> b) -> [a] -> [[a]]
groupOnNonAdjacent mapping list = groupBy (on (==) mapping) $ sortOn mapping list

-- | Transforms all the letters in the given String to upper case.
toUpperCase :: String -> String
toUpperCase = map toUpper

-- | Get matches in the given list, mapping the elements with the given function.
--   A match is considered to be found when the mapping is equal to the given value.
--   When matches are found, then the first element in the matching list is returned.
getMatches :: Eq b => (a -> b) -> b -> [a] -> Maybe a
getMatches f v list = if null matches
    then Nothing
    else Just . head $ matches
    where
        matches = filter ((==) v . f) list
