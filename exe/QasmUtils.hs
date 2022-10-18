-- | Utilities to read OpenQASM 3 programs.

module QasmUtils
  ( parseQasmFromArgs
  ) where

import Qasm.Language (Stmt(..))
import Qasm.Parser (parseQasm)
import System.Environment (getArgs)

-- |
parseQasmFromArgs :: IO (Either String [Stmt])
parseQasmFromArgs = do
    args <- getArgs
    case args of
        []  -> fmap (parseQasm "<stdin>") getContents
        [f] -> fmap (parseQasm f) (readFile f)
