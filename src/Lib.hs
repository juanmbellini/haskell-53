module Lib
    ( toIOJust
    , groupOnNonAdjacent
    ) where


import Data.List
import Data.Function    (on)


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
