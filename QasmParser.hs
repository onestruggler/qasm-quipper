-- This file is part of Quipper. Copyright (C) 2011-2019. Please see the
-- file COPYRIGHT for a list of authors, copyright holders, licensing,
-- and other details. All rights reserved.
-- 
-- ======================================================================

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- ----------------------------------------------------------------------
-- | This program reads an execution trace produced by OpenQasm, and turns
-- it into a Quipper circuit.

module Main where

import Quipper hiding (cnot, Format)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as Trav
import Control.Monad.State
import Prelude hiding (not)
import qualified Prelude
import System.Environment
import System.Exit
import System.IO
import Data.Complex
import Data.Maybe

import Text.ParserCombinators.ReadP hiding (get)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Text as T

-- ----------------------------------------------------------------------
-- * A monad for a OpenQasm state

-- | In OpenQasm, qubits are identified by array names and array
-- indices (integers). Classical bits are represented in the same
-- way. We have to map these to Quipper's native qubits. A
-- 'OpenQasmState' holds such a map.  Implicitly, it also holds the
-- set of qubits currently defined.
data OpenQasmRegType =  Qreg | Creg deriving (Eq, Ord, Show)
type RegType = OpenQasmRegType

-- (OpenQasmRegType, array name, array size/array index).  When used
-- in Array declaration, Int represents the size. When used as pointer
-- to qubit, Int represents the index.
type OpenQasmQCreg = (RegType, String, Int)
type OQCbit = OpenQasmQCreg

type OpenQasmState = Map OpenQasmQCreg Qubit

-- | The 'OpenQasmCirc' monad is like the 'Circ' monad, except that it also
-- keeps track of an additional 'OpenQasmState'. The 'lift' function must
-- be used to lift any command for the 'Circ' monad to the 'OpenQasmCirc'
-- monad.
type OpenQasmCirc a = StateT OpenQasmState Circ a

-- ----------------------------------------------------------------------
-- * Auxiliary state functions

-- | Look up the qubit corresponding to a OpenQasm register, or allocate a
-- new qubit if it doesn't already exist.
provide :: OQCbit -> OpenQasmCirc Qubit
provide r = do
  s <- get
  case Map.lookup r s of
    Just q -> return q
    Nothing -> do
      q <- lift $ qinit False
      let s' = Map.insert r q s
      put s'
      return q

-- | Look up qubits corresponding to OpenQasm registers, or allocate
-- new qubits if it any of them doesn't already exist.
provides :: [OQCbit] -> OpenQasmCirc [Qubit]
provides [] = return []
provides (r:t) = do
    r' <- provide r
    t' <- provides t
    return $ r' : t'


-- ----------------------------------------------------------------------
-- * Implementation of the OpenQasm primitives

-- | Reset all qubits to state 0.
qasm_reset' :: OpenQasmCirc ()
qasm_reset' = do
  s <- get
  lift $ Trav.mapM qdiscard s
  let s' = Map.empty
  put s'
  return ()

-- | Reset a list of qubits to state 0.
qasm_reset_qlist :: [OQCbit] -> OpenQasmCirc ()
qasm_reset_qlist ql = do
  s <- get
  if all (\x -> Map.member x s) ql then do
    let ql' = Map.filterWithKey (\k a -> k `elem` ql) s
    lift $ Trav.mapM (qterm False) ql'
    let s' = foldr (Map.delete) s ql
    put s'
    return ()
  else do
    error "qasm_reset_qlist: reset non-exist qubits"

-- | Init a list of qubits to state 0.
qasm_init_qlist :: [OQCbit] -> OpenQasmCirc ()
qasm_init_qlist ql = do
  s <- get
  if all (\x -> Map.member x s) ql then do
    let ql' = Map.filterWithKey (\k a -> k `elem` ql) s
    lift $ Trav.mapM qinit $ replicate (length ql) False
    let s' = foldr (Map.delete) s ql
    put s'
    return ()
  else do
    error "qasm_init_qlist: init non-exist qubits"


-- | Reset one qubit to state 0.
qasm_reset :: OQCbit -> OpenQasmCirc ()
qasm_reset q = qasm_reset_qlist [q]

-- | Apply a controlled-not operation to the first argument.
qasm_cnot :: OQCbit -> [OQCbit] -> OpenQasmCirc ()
qasm_cnot r ctrls = do
  q <- provide r
  cs <- Trav.mapM provide ctrls
  lift $ qnot_at q `controlled` cs
  return ()

-- | Apply an uncontrolled not operation.
qasm_not :: OQCbit -> OpenQasmCirc ()
qasm_not r = qasm_cnot r []

qasm_x = qasm_not

-- | Apply an one-controlled not operation.
qasm_cx :: OQCbit -> OQCbit -> OpenQasmCirc ()
qasm_cx r c = qasm_cnot r [c]

-- | Apply an two-controlled not operation, toffoli gate.
qasm_tof :: OQCbit -> OQCbit -> OQCbit -> OpenQasmCirc ()
qasm_tof r c1 c2 = qasm_cnot r [c1, c2]

qasm_ccx = qasm_tof

-- | The inverse of 'qasm_cnot'.
qasm_cnot_inv = qasm_cnot

-- | The inverse of 'qasm_not'.
qasm_not_inv = qasm_not

-- | A sample circuit to illustrate how to use the primitives.
testcircuit1 :: OpenQasmCirc ()
testcircuit1 = do
  qasm_reset (Qreg, "q", 3)
  qasm_x (Qreg, "q", 2)
  qasm_cx (Qreg, "q", 2) (Qreg, "q2", 2)

-- ----------------------------------------------------------------------
-- * Unpacking OpenQasmCirc

-- | Run function for the 'OpenQasmCirc' monad: execute the actions and
-- produce a circuit.
run :: OpenQasmCirc a -> Circ a
run f = do
  (x,_) <- runStateT f Map.empty
  return x

-- ----------------------------------------------------------------------
-- * An abstract syntax for OpenQasm output

-- | A data type to hold a OpenQasm gate.
data OpenQasmGate = 
  Comment String
  | QCreg OQCbit
  | X OQCbit
  | CX OQCbit OQCbit
  | CCX OQCbit OQCbit OQCbit
  | Reset OQCbit
  | Not OQCbit
  | XNot OQCbit
  | CNot OQCbit [OQCbit]
  | XCNot OQCbit [OQCbit]
  | Fanout [OQCbit] [OQCbit] [OQCbit]
  | XFanout [OQCbit] [OQCbit] [OQCbit]
  | Matrix OQCbit [Complex Double] [OQCbit]
  | XMatrix OQCbit [Complex Double] [OQCbit]
  deriving (Show)

-- | Take a gate from the abstract syntax and execute it in the
-- 'OpenQasmCirc' monad.
do_qasm_gate :: OpenQasmGate -> OpenQasmCirc ()
do_qasm_gate (Comment s) = return ()
do_qasm_gate (QCreg (t, id, s)) = do
  let ql = map (\x -> (t, id, x)) [0..s-1]
  provides ql
  return () 
do_qasm_gate (Reset x) = qasm_reset x
do_qasm_gate (X x) = qasm_x x
do_qasm_gate (CX x c1) = qasm_cx x c1
do_qasm_gate (CCX x c1 c2) = qasm_ccx x c1 c2

-- ----------------------------------------------------------------------
-- * Parsing

-- $ The output of OpenQasm consists of lines of the following forms. 
-- 
-- OPENQASM 2.0;
--include "qelib1.inc";

-- qreg in[16];

-- cx in[7],in[15];
-- qreg anc16[1];
-- x in[15];
-- ccx in[7],in[15],anc16[0];
-- 
-- We use Koen Claessen's parser combinators (see
-- "Text.ParserCombinators.ReadP") to implement the parser.

-- | Parse a OpenQasm identifier, which we take to be a non-empty string of
-- alphanumeric characters, starting with a letter
identifier :: ReadP String
identifier = do
  satisfy isAlpha
  munch isAlphaNum
  
-- | Parse a signless integer. We avoid the usual trick ('readS_to_P'
-- 'reads'), because this introduces backtracking errors.
int :: ReadP Int
int = do
  s <- munch1 isDigit
  return $ (read s :: Int)

-- | Parse a floating point number. We avoid the usual trick
-- ('readS_to_P' 'reads'), because this introduces backtracking
-- errors.
double :: ReadP Double
double = do
  (s, _) <- gather parse_double
  return $ (read s :: Double)
  where
    parse_double = do
      option '+' (char '+' +++ char '-')
      munch isDigit
      optional (char '.' >> munch1 isDigit)
      
-- | Parse a comma separated list.
commalist :: ReadP a -> ReadP [a]
commalist elt = sepBy elt (skipSpaces >> char ',' >> skipSpaces)

-- | Parse a OpenQasm quantum register of the form 
-- 
-- qreg q[16]
-- creg q[16]
qreg :: ReadP OQCbit
qreg = do
  skipSpaces
  string "qreg"
  skipSpaces
  id <- identifier
  char '['
  skipSpaces
  reg_size <- int
  skipSpaces
  char ']'
  skipSpaces
  char ';'
  return (Qreg, id, reg_size)

creg :: ReadP OQCbit
creg = do
  skipSpaces
  string "creg"
  skipSpaces
  id <- identifier
  char '['
  skipSpaces
  reg_size <- int
  skipSpaces
  char ']'
  skipSpaces
  char ';'
  return (Creg, id, reg_size)


oqbit :: ReadP OQCbit
oqbit = do
  id <- identifier
  char '['
  skipSpaces
  index <- int
  skipSpaces
  char ']'
  return (Qreg, id, index)

ocbit :: ReadP OQCbit
ocbit = do
  id <- identifier
  char '['
  skipSpaces
  index <- int
  skipSpaces
  char ']'
  return (Creg, id, index)


-- | Consume an optional \"!\". Return 'True' if consumed, and 'False'
-- otherwise.
inversechar :: ReadP Bool  
inversechar = do
  c <- option '+' (char '!')
  return (c == '!')

-- | Parse a complex number declaration of the format
-- 
-- > complex u00=1
-- 
-- or
-- 
-- > complex u22=(0.995004,-0.0998334).
complex :: ReadP (Complex Double)
complex = do
  string "complex"
  skipSpaces
  identifier
  skipSpaces
  char '='
  skipSpaces
  choice [
    do 
      x <- double
      return (x :+ 0)
    ,
    do
      char '('
      skipSpaces
      x <- double  
      skipSpaces
      char ','
      skipSpaces
      y <- double
      skipSpaces
      char ')'
      return (x :+ y)
    ]

-- | Parse OpenQasm 2.0 header.
qasm_header1 :: ReadP String
qasm_header1 = do
  skipSpaces
  s1 <- string "OPENQASM"
  skipSpaces
  s2 <- string "2.0"
  skipSpaces
  char ';'
  return $ s1 ++ " " ++ s2


qasm_header2 :: ReadP String
qasm_header2 = do
  skipSpaces
  s1 <- string "include"
  skipSpaces
  s2 <- string "\"qelib1.inc\""
  skipSpaces
  char ';'
  return $ s1 ++ " " ++ s2


-- | Parse a single line of OpenQasm output into a 'OpenQasmGate'.
qasm_line :: ReadP OpenQasmGate
qasm_line = choice [
  do 
    skipSpaces
    x <- qreg
    return (QCreg x)
  ,
  do 
    skipSpaces
    x <- creg
    return (QCreg x)
  ,
  do -- @ Reset
    skipSpaces
    string "reset"
    skipSpaces
    q <- oqbit
    skipSpaces
    char ';'
    return (Reset q)
  ,
  do -- @ NOT(qureg q=<5>)
    skipSpaces
    string "x"
    skipSpaces
    q <- oqbit
    skipSpaces
    char ';'
    return (X q)
  ,
  do -- @ NOT(qureg q=<5>)
    skipSpaces
    string "cx"
    skipSpaces
    c <- oqbit
    skipSpaces
    char ','
    skipSpaces
    t <- oqbit
    skipSpaces
    char ';'
    return (CX t c)
  ,
  do -- @ NOT(qureg q=<5>)
    skipSpaces
    string "ccx"
    skipSpaces
    c1 <- oqbit
    skipSpaces
    char ','
    skipSpaces
    c2 <- oqbit
    skipSpaces
    char ','
    skipSpaces
    t <- oqbit
    skipSpaces
    char ';'
    return (CCX t c1 c2)
  ,
  do -- any line starting with '//' is a comment
    skipSpaces
    char '/'
    char '/'
    str <- munch (\x -> True)
    return (Comment str)
  ,
  do -- empty lines are comments
    eof
    return (Comment "")
  ]

-- | String version of 'qasm_line': parse a string and turn it into a
-- 'OpenQasmGate'.
parse_qasm_line :: String -> OpenQasmGate
parse_qasm_line s =
  case readP_to_S readline s of
    (h, _):_ -> h
    _ -> error ("Unrecognized line: " ++ s)
  where
    readline = do
      skipSpaces
      l <- qasm_line
      skipSpaces
      eof
      return l      
      
-- | Monad version of 'parse_qasm_line': parse a string and execute the
-- resulting gate directly in the 'OpenQasmCirc' monad.
run_qasm_line :: String -> OpenQasmCirc ()
run_qasm_line = do_qasm_gate . parse_qasm_line

-- | Parse a stream consisting of many lines of OpenQasm output and execute
-- the parsed gates in the 'OpenQasmCirc' monad.
run_qasm_lines :: String -> OpenQasmCirc ()
run_qasm_lines s =
  mapM_ run_qasm_line (lines s)

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = (x ++ take 1 y) : split d (drop 1 y) where (x,y) = span (/= d) s

-- | Parse a stream of OpenQasm output and execute the parsed gates in
-- the 'OpenQasmCirc' monad.
run_qasm_file :: String -> OpenQasmCirc ()
run_qasm_file s = do
  let ss = split ';' s
  let ss' = map (T.unpack . T.strip . T.pack) ss
  let ls = concat $ map lines ss'
  let header1 = ls !! 0
  let header1 = ls !! 1
  mapM_ run_qasm_line $ drop 2 ls


-- | A sample circuit to illustrate the parser.
testcircuit2 :: OpenQasmCirc ()
testcircuit2 = do
  run_qasm_line "cx in[1], in[2];"
  run_qasm_line "reset in[3];"
  run_qasm_line "ccx in[3], in[4], in[5]"

-- ----------------------------------------------------------------------
-- * Main function

-- | Print a usage message to 'stdout'.
usage :: IO ()
usage = do
  name <- getProgName
  putStr (header name)
    where header name =
            name ++ ": read an restricted OpenQasm 2.0 file,\n"
            ++ "and turn it into a Quipper circuit.\n"

-- | Main function: read a circuit in OpenQasm format from 'stdin', and
-- preview the translated Quipper circuit.
main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [] -> return ()
    "-h" : _ -> do
      usage
      exitSuccess
    "--help" : _ -> do
      usage
      exitSuccess
    o : _ -> do
      hPutStrLn stderr ("Bad argument or option: '" ++ o ++ "'. Try --help for more info.")
      exitFailure

  lines <- hGetContents stdin
  print_simple ASCII (run (run_qasm_file lines))

