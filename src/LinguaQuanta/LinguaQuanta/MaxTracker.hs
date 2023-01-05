-- | General-purpose utility functions.

module LinguaQuanta.MaxTracker
  ( MaxTracker
  , curval
  , initTracker
  , maxval
  , updateTracker
  ) where

-------------------------------------------------------------------------------
-- * Computing Maxima.

-- | The MaxTracker maintains, and allows updates to, an ordered value. Across
-- all updates to the value, the maximum value is maintained.
data (Ord a) => MaxTracker a = MaxTracker { curval :: a
                                          , maxval :: a
                                          } deriving (Eq, Show)

-- | Returns a MaxTracker initialized to the input value.
initTracker :: (Ord a) => a -> MaxTracker a
initTracker v = MaxTracker v v

-- | Consumes a MaxTracker, together with an endomorphism on the MaxTracker
-- type. Returns a new MaxTracker, obtained by applying the endomorphism to the
-- wrapped value, and then updating the maximum accordingly.
updateTracker :: (Ord a) => MaxTracker a -> (a -> a) -> MaxTracker a
updateTracker (MaxTracker v m) f = MaxTracker v' (max m v')
    where v' = f v
