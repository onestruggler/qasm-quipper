-- Monad template from: https://github.com/dagit/happy-plus-alex/
{
module Lexer
  ( Token(..)
  , TokenClass(..)
  , unlex
  , Alex(..)
  , runAlexQasm
  , alexQasmMonadScan
  , alexQasmError
  ) where

import Control.Monad (liftM)
import System.FilePath
}

%wrapper "monadUserState"

$decimal    = [0-9]
$alpha      = [A-Za-z]
$greek      = [\x370-\x3FF]
$idchars    = ['_' $alpha $greek]

tokens :-
    $white+                                 ;
    ctrl                                    { constLex TokenCtrl }
    negctrl                                 { constLex TokenNegCtrl }
    inv                                     { constLex TokenInv }
    pow                                     { constLex TokenPow }
    ($decimal '_'?)* $decimal               { charLex TokenDecInt }
    \x3C0 | pi                              { charLex TokenPi }
    $idchars [$idchars $decimal]*           { charLex TokenID }
    \@                                      { constLex TokenAt }
    \+                                      { constLex TokenPlus }
    \-                                      { constLex TokenMinus }
    \*                                      { constLex TokenStar }
    \/                                      { constLex TokenSlash }
    \(                                      { constLex TokenLParen }
    \)                                      { constLex TokenRParen }
    \[                                      { constLex TokenLBrack }
    \]                                      { constLex TokenRBrack }
    \;                                      { constLex TokenSemicolon }

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
