-- | Converts a Quipper program into our internal representation.

module Quip.Parser
  ( QuipCirc(..)
  , parseQuip
  ) where

import Quipper (Endpoint, Circ)
import Quipper.Libraries.QuipperASCIIParser (parse_circuit)

-------------------------------------------------------------------------------
-- * Functional Circuit Representation.

-- | Wraps a Quipper circuit function, along with its shape.
data QuipCirc = QuipCirc { shape :: [Endpoint]
                         , qfunc :: [Endpoint] -> Circ [Endpoint]
                         }

-------------------------------------------------------------------------------
-- * Circuit Parsing.

-- | Consumes the name of an input stream (fp) and the contents of the input
-- stream (input). If input contaisn a valid Quipper circuit in ASCII format,
-- then the Quipper circuit is returned. Otherwise, an error is raised.
parseQuip :: FilePath -> String -> QuipCirc
parseQuip fp input = QuipCirc { shape = sp, qfunc = fn }
    where (sp, fn) = parse_circuit input
