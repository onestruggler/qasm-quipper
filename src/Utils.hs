-- | General-purpose utility functions.

module Utils
  ( maybeAppend
  , maybeWrap
  , setMaybe
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.Maybe
  ( isNothing
  , maybe
  )

-------------------------------------------------------------------------------
-- * Maybe Utilities.

-- | Takes as input a value v of type T and maybe a value u also of type T. If
-- v is nothing, then just u is returned. Otherwise, v is returned.
setMaybe :: a -> Maybe a -> Maybe a
setMaybe u Nothing = Just u
setMaybe _ v       = v

-- | Takes as input maybe a value v. If v is just a value, then just [v] is
-- returned. Otherwise, nothing is returned.
maybeWrap :: Maybe a -> Maybe [a]
maybeWrap = maybe Nothing (\x -> Just [x])

-- | Takes as input maybe a list of values vs and a value v. If vs is just a
-- list, then just v:vs is returned. Otherwise, nothing is returned.
maybeAppend :: a -> Maybe [a] -> Maybe [a]
maybeAppend v = maybe Nothing (\vs -> Just (v:vs))
