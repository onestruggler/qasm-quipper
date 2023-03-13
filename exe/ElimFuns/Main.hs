-- | Command-line interface to the OpenQASM 2.0 function elimination pass.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import ElimFuns.CmdLn
  ( ElimFunsTool(..)
  , getToolArgs
  )
import LinguaQuanta.Either
  ( expandLeft
  , expandRight
  )
import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Passes (elimFun)
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
        \ast -> expandRight (elimFun ast) $ \err -> Right $ show err

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ElimFunsTool -> IO ()
processArgs mode@ElimFuns{..} = setupTool doTask display src out
    where display = printQasmAST False

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
