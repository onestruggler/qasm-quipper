-- | Utility functions for Either types.

module LinguaQuanta.Either
  ( expandLeft
  , leftMap
  ) where

-------------------------------------------------------------------------------
-- * Either Mapping.

-- | Applies a two-valued function to the left branch of an either value, such
-- that the left and right branches will unify.
expandLeft :: Either a c -> (a -> Either b c) -> Either b c
expandLeft (Left x)  f = f x
expandLeft (Right y) _ = Right y

-- | Takes as input a two-valued function (f) and a list of arguments to f. If
-- f maps every element of the list to a left value, then returns a new list of
-- those left values, in the order of the original list. Otherwise, if a single
-- list element maps to a right value, then returns the first such right value.
--
-- Typically, f is a function with nominal (left) and exceptional (right)
-- output, and maybeMap acts as a mapping functor with support for exceptions.
leftMap :: (a -> Either b c) -> [a] -> Either [b] c
leftMap _ []     = Left []
leftMap f (x:xs) =
    expandLeft (f x) $
        \x' -> expandLeft (leftMap f xs) $
            \xs' -> Left $ x':xs'
