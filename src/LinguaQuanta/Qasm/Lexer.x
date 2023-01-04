-- Monad template from: https://github.com/dagit/happy-plus-alex/
{
module LinguaQuanta.Qasm.Lexer
  ( Token(..)
  , TokenClass(..)
  , unlex
  , Alex(..)
  , runAlexQasm
  , alexQasmMonadScan
  , alexQasmError
  ) where

import Control.Monad (liftM)
import System.FilePath (FilePath)
}

%wrapper "monadUserState"

$decimal    = [0-9]
$alpha      = [A-Za-z]
$greek      = [\x370-\x3FF]
$idchars    = ['_' $alpha $greek]

tokens :-
    <0>             $white+                           ;
    -- Comment Parsing.
    <0>             \/\/                              { begin commentsl }
    <0>             \/\*                              { begin commentml }
    <commentsl>     \n                                { begin 0 }
    <commentsl>     .                                 ;
    <commentml>     \*\/                              { begin 0 }
    <commentml>     [.\n]                             ;
    -- Gates and Modifiers.
    <0>             ctrl                              { constLex TokenCtrl }
    <0>             negctrl                           { constLex TokenNegCtrl }
    <0>             inv                               { constLex TokenInv }
    <0>             pow                               { constLex TokenPow }
    <0>             gphase                            { constLex TokenGPhase }
    -- Type Keywords.
    <0>             qreg                              { constLex TokenQReg }
    <0>             qubit                             { constLex TokenQubit }
    -- Literals and Identifiers.
    <0>             ($decimal '_'?)* $decimal         { charLex TokenDecInt }
    <0>             \x3C0 | pi                        { charLex TokenPi }
    <0>             $idchars [$idchars $decimal]*     { charLex TokenID }
    -- Operators.
    <0>             \@                                { constLex TokenAt }
    <0>             \+                                { constLex TokenPlus }
    <0>             \-                                { constLex TokenMinus }
    <0>             \*                                { constLex TokenStar }
    <0>             \/                                { constLex TokenSlash }
    -- Braces.
    <0>             \(                                { constLex TokenLParen }
    <0>             \)                                { constLex TokenRParen }
    <0>             \[                                { constLex TokenLBrack }
    <0>             \]                                { constLex TokenRBrack }
    <0>             \,                                { constLex TokenComma }
    <0>             \;                                { constLex TokenSemicolon }

{
-- File path is maintained to improve error messages.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The tokens returned by the parser.
data TokenClass = TokenCtrl
                | TokenNegCtrl
                | TokenInv
                | TokenPow
                | TokenGPhase
                | TokenQReg
                | TokenQubit
                | TokenDecInt String
                | TokenPi String
                | TokenID String
                | TokenAt
                | TokenPlus
                | TokenMinus
                | TokenStar
                | TokenSlash
                | TokenLParen
                | TokenRParen
                | TokenLBrack
                | TokenRBrack
                | TokenComma
                | TokenSemicolon
                | TokenEOF
                deriving (Show)

data Token = Token AlexPosn TokenClass deriving (Show)

-- Converts tokens into strings for nicer error messages.
unlex :: TokenClass -> String
unlex TokenCtrl       = "ctrl"
unlex TokenNegCtrl    = "negctrl"
unlex TokenInv        = "inv"
unlex TokenPow        = "pow"
unlex TokenGPhase     = "gphase"
unlex TokenQReg       = "qreg"
unlex TokenQubit      = "qubit"
unlex (TokenDecInt x) = (show x)
unlex (TokenPi tok)   = (show tok)
unlex (TokenID str)   = (show str)
unlex TokenAt         = "@"
unlex TokenPlus       = "+"
unlex TokenMinus      = "-"
unlex TokenStar       = "*"
unlex TokenSlash      = "/"
unlex TokenLParen     = "("
unlex TokenRParen     = ")"
unlex TokenLBrack     = "["
unlex TokenRBrack     = "]"
unlex TokenComma      = ","
unlex TokenSemicolon  = ";"
unlex TokenEOF        = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
    (p, _, _, _) <- alexGetInput
    return $ Token p TokenEOF

-- Now we must extract the strings and prepare the tokens.
charLex :: (String -> TokenClass) -> AlexAction Token
charLex f = \(p, _, _, s) i -> return $ Token p (f (take i s))

-- Inclusion for tokens that do not depend on input.
constLex :: TokenClass -> AlexAction Token
constLex = charLex . const

-- Error message generation.
alexQasmError :: AlexPosn -> String -> Alex a
alexQasmError (AlexPn _ l c) msg = do
    fp <- getFilePath
    alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- Improved error messages from the scanner.
alexQasmMonadScan :: Alex Token
alexQasmMonadScan = do
    inp <- alexGetInput
    sc <- alexGetStartCode
    case alexScan inp sc of
        AlexEOF -> alexEOF
        AlexError (p, _, _, s) ->
            alexQasmError p ("lexical error at character '" ++ take 1 s ++ "'")
        AlexSkip inp' len -> do
            alexSetInput inp'
            alexQasmMonadScan
        AlexToken inp' len action -> do
            alexSetInput inp'
            action (ignorePendingBytes inp) len

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlexQasm :: Alex a -> FilePath -> String -> Either String a
runAlexQasm a fp input = runAlex input (setFilePath fp >> a)
}
