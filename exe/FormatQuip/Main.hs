-- | Command-line interface to visualize Quipper programs.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import FormatQuip.CmdLn
  ( FormatQuipTool(..)
  , getToolArgs
  )
import LinguaQuantaExe.CmdLnFlags (QuipVizFormat(..))
import LinguaQuantaExe.IOUtils (redirectStdout)
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , DisplayFn
  , setupTool
  )
import Quipper
  ( Circ
  , Endpoint
  )
import qualified Quipper.Internal.Printing (Format(..))
import Quipper.Internal.Printing (print_unary)
import Quipper.Libraries.QuipperASCIIParser (parse_circuit)

-------------------------------------------------------------------------------
-- * FormatQuip Interface.

-- |
type FunctionQuipCirc = ([Endpoint], [Endpoint] -> Circ [Endpoint])

-- |
toFormat :: QuipVizFormat -> Quipper.Internal.Printing.Format
toFormat PDF       = Quipper.Internal.Printing.PDF
toFormat PS        = Quipper.Internal.Printing.PS
toFormat EPS       = Quipper.Internal.Printing.EPS
toFormat GateCount = Quipper.Internal.Printing.GateCount

parseQuip :: DoTaskFn FunctionQuipCirc
parseQuip _ input = Left $ parse_circuit input

printQuip :: QuipVizFormat -> DisplayFn FunctionQuipCirc
printQuip viz hdl (sp, fn) =
    redirectStdout hdl $ \() -> do
        print_unary format fn sp
    where format = toFormat viz

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: FormatQuipTool -> IO ()
processArgs mode@FormatQuip{..} = setupTool parseQuip display src out
    where display = printQuip viz

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
