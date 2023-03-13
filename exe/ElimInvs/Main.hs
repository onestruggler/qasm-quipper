-- | Command-line interface to the OpenQASM 3 inv modifier elimination pass.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import ElimInvs.CmdLn
  ( ElimInvsTool(..)
  , getToolArgs
  )
import LinguaQuanta.Either
  ( expandLeft
  , expandRight
  )
import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Passes (elimInv)
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , DisplayFn
  , setupTool
  )
import LinguaQuantaExe.QasmUtils
  ( parseQasmAST
  , printQasmAST
  )

-------------------------------------------------------------------------------
-- * ElimInvs Interface.

doTask :: DoTaskFn [AstStmt]
doTask file input =
    expandLeft (parseQasmAST file input) $
        \ast -> expandRight (elimInv ast) $ \err -> Right $ show err

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ElimInvsTool -> IO ()
processArgs mode@ElimInvs{..} = setupTool doTask display src out
    where display = printQasmAST False

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
