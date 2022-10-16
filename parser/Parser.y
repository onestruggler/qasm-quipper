-- Monad template from: https://github.com/dagit/happy-plus-alex/
{
module Parser (parseQasm) where

import Lexer
import QasmLang
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
    ctrl            { Token _ TokenCtrl }
    negctrl         { Token _ TokenNegCtrl }
    inv             { Token _ TokenInv }
    pow             { Token _ TokenPow }
    decint          { Token _ (TokenDecInt $$) }
    pi              { Token _ (TokenPi $$) }
    id              { Token _ (TokenID $$) }
    '@'             { Token _ TokenAt }
    '+'             { Token _ TokenPlus }
    '-'             { Token _ TokenMinus }
    '*'             { Token _ TokenStar }
    '/'             { Token _ TokenSlash }
    '('             { Token _ TokenLParen }
    ')'             { Token _ TokenRParen }
    '['             { Token _ TokenLBrack }
    ']'             { Token _ TokenRBrack }
    ';'             { Token _ TokenSemicolon }

%left '+' '-'
%left '*' '/'
%left NEG
%%

Gate : id GateOperands                      { NamedGate $1 $2 }
     | ctrl '@' Gate                        { CtrlMod Nothing $3 }
     | ctrl '(' Expr ')' '@' Gate           { CtrlMod (Just $3) $6 }
     | negctrl '@' Gate                     { NegCtrlMod Nothing $3 }
     | negctrl '(' Expr ')' '@' Gate        { NegCtrlMod (Just $3) $6 }
     | inv '@' Gate                         { InvMod $3 }
     | pow '(' Expr ')' '@' Gate            { PowMod $3 $6 }

Expr : Expr '+' Expr                        { Plus $1 $3 }
     | Expr '-' Expr                        { Minus $1 $3 }
     | Expr '*' Expr                        { Times $1 $3 }
     | Expr '/' Expr                        { Div $1 $3 }
     | '(' Expr ')'                         { Brack $2 }
     | '-' Expr %prec NEG                   { Negate $2 }
     | pi                                   { Pi}
     | decint                               { DecInt $1 }
     | id                                   { QasmId $1 }

GateOperand : id                            { QVar $1 }
            | id '[' Expr ']'               { QReg $1 $3 }

GateOperands : GateOperand                  { [$1] }
             | GateOperand GateOperands     { $1 : $2 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexQasmMonadScan >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexQasmError p ("parse error at token '" ++ unlex t ++ "'")

parseQasm :: FilePath -> String -> Either String Gate
parseQasm = runAlexQasm parse
}
