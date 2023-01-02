-- | Functions and data types to track the allocation states of OpenQASM
-- registers, and to map said registers to Quipper wires.

module QasmToQuip.Wire
  ( WireState
  , WireError(..)
  , Show(..)
  , Eq(..)
  , allocateWire
  , isInput
  , initWire
  , isOutput
  , termWire
  , useWire
  , wireIndex
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.Maybe
  ( isJust
  , maybe
  )
import Utils (setMaybe)

-------------------------------------------------------------------------------
-- * Wire Allocation Data.

-- | Each OpenQASM register is mapped to a Quipper wire. The state of this wire
-- is represented by a WireState.
data WireState = WireState { wireIndex :: Int
                           , initFirst ::(Maybe Bool)
                           , termAtEnd :: Bool
                           } deriving (Show, Eq)

-- | Consumes an integer n. Initializes a new wire, with label n, that is not
-- yet initialized nor terminated. Initializing a wire from this state will
-- create an ancilla. Otherwise, the wire is taken to be an input to the
-- circuit. Likewise, the wire is taken to be an output unless the wire is
-- eventually terminated.
allocateWire :: Int -> WireState
allocateWire n = WireState { wireIndex = n
                           , initFirst = Nothing
                           , termAtEnd = False
                           }

-- | Returns true if the wire is taken as an input.
isInput :: WireState -> Bool
isInput = maybe True not . initFirst

-- | Returns true if the wire is returned as an output.
isOutput :: WireState -> Bool
isOutput = not . termAtEnd

-------------------------------------------------------------------------------
-- * Abstract Wire Operations (WireState API).

-- | Errors representing improper usage of the WireState API.
data WireError = DoubleInit
               | DoubleTerm
               | UseBeforeInit
               deriving (Show, Eq)

-- | A function that takes as input the state of a wire. If the wire update is
-- valid from the given state, then a new wire state is returned. Otherwise, a
-- wire error is returned which explains the failure.
type WireUpdate = WireState -> Either WireError WireState

-- | Internal implementation of useWire, initWire, and termWire. Consumes two
--Bbooleans, indicating initialization and termination, respectively. Returns a
-- WireUpdate function defined by the two Booleans.
updateWire :: Bool -> Bool -> WireUpdate
updateWire init term state
    -- These states should be unreachable and indicate API usage errors.
    | init && active && used = Left DoubleInit
    | term && not active     = Left DoubleTerm
    | not init && not active = Left UseBeforeInit
    -- Otherwise, update the state.
    | otherwise = Right WireState { wireIndex = wireIndex state
                                  , initFirst = setMaybe init (initFirst state)
                                  , termAtEnd = term
                                  }
    where active = isOutput state
          used   = isJust $ initFirst state

-- | Computes the state of a wire, after applying either a measurement or a
-- unitary gate. If the wire is not active, then an error is returned.
useWire :: WireUpdate
useWire = updateWire False False

-- | Computes the state of a wire, after (re)initializing it as an ancilla. If
-- the wire is already in use, then an error is returned.
initWire :: WireUpdate
initWire = updateWire True False

-- | Computes the state of a wire, after terminating it. If the wire is not in
-- use, then an error is returned.
termWire :: WireUpdate
termWire = updateWire False True
