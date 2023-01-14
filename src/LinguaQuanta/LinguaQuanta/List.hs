-- | List functions not provided by Prelude nor Data.List.

module LinguaQuanta.List (splitAtFirst) where

-------------------------------------------------------------------------------
-- * String Manipulation.

-- | Takes as input a predicate over values of type a (f), and a list of values
-- of type a (xs). Returns a pair of lists obtained by splitting xs at the
-- first element satisfying f. Formally, if no elements of xs satsify f, then
-- the tuple (xs, []) is returned. Otherwise, if xs = ys:z:ws and z is the
-- first element of xs such that z satisfies f, then the tuple (ys, ws) is
-- returned. In particular, if xs = [], then ([], []) is returned.
splitAtFirst :: (Eq a) => (a -> Bool) -> [a] -> ([a], [a])
splitAtFirst _ [] = ([], [])
splitAtFirst f (x:xs)
    | f x       = ([], xs)
    | otherwise = let (pre, post) = splitAtFirst f xs in (x:pre, post)
