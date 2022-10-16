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
    gphase          { Token _ TokenGPhase }
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
    ','             { Token _ TokenComma }
    ';'             { Token _ TokenSemicolon }

%left '+' '-'
%left '*' '/'
%left NEG
%%

StmtList : Stmt                             { [$1] }
         | Stmt StmtList                    { $1 : $2 }

Stmt : Gate ';'                             { QasmGateStmt $1 }

Gate : id GateOperands                      { NamedGateOp $1 [] $2 }
     | id '(' ExprList ')' GateOperands     { NamedGateOp $1 $3 $5 }
     | gphase '(' Expr ')'                  { GPhaseOp $3 [] }
     | gphase '(' Expr ')' GateOperands     { GPhaseOp $3 $5 }
     | ctrl '@' Gate                        { CtrlMod Nothing $3 }
     | ctrl '(' Expr ')' '@' Gate           { CtrlMod (Just $3) $6 }
     | negctrl '@' Gate                     { NegCtrlMod Nothing $3 }
     | negctrl '(' Expr ')' '@' Gate        { NegCtrlMod (Just $3) $6 }
     | inv '@' Gate                         { InvMod $3 }
     | pow '(' Expr ')' '@' Gate            { PowMod $3 $6 }

ExprList : Expr                             { [$1] }
         | Expr ',' ExprList                { $1 : $3 }

Expr : Expr '+' Expr                        { Plus $1 $3 }
     | Expr '-' Expr                        { Minus $1 $3 }
     | Expr '*' Expr                        { Times $1 $3 }
     | Expr '/' Expr                        { Div $1 $3 }
     | '(' Expr ')'                         { Brack $2 }
     | '-' Expr %prec NEG                   { Negate $2 }
     | pi                                   { Pi }
     | decint                               { DecInt $1 }
     | id                                   { QasmId $1 }

GateOperands : GateOperand                  { [$1] }
             | GateOperand GateOperands     { $1 : $2 }

GateOperand : id                            { QVar $1 }
            | id '[' Expr ']'               { QReg $1 $3 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexQasmMonadScan >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexQasmError p ("parse error at token '" ++ unlex t ++ "'")

parseQasm :: FilePath -> String -> Either String [Stmt]
parseQasm = runAlexQasm parse
}
