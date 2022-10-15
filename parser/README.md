# OpenQASM 3 Parser

A simple parser for OpenQASM 3.
Consists of a lexer, parser, and a method to convert the concrete parse tree into a workable abstract syntax tree (AST).

## Instructions

To build a standalone parser:
1. Ensure that `alex` and `happy` are installed.
2. Run `alex Lexer.x`.
3. Run `happy Parser.y`.
4. Run `ghc Main.hs`.

