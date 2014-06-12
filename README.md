# VParser

Variational Monadic Parser Combinator Library. See my June 2014 OSU MS Project 
Report for theoretical background.

All of this code was developed on and tested against GHC 7.6.3.

## Main Codebase (root directory)

The main codebase consists of variational data representations (VData.hs), a 
variational parser combinator library coded against those representations 
(VParser.hs), and two example applications of the parser (VLCLang.hs and 
VJSLang.hs).

### VStringReader.hs

VStringReader.hs is a tool for importing variational sorce code from a file. For
example, if a file named script.js.v contained the text:

    function f(x) { return @A<x*x@,Math.pow(x,2)@>; }

VStringReader.hs provides the function `vstringFromFile :: FilePath -> IO 
VString` which loads it and parses it into a `VString` type.

    > vstringFromFile "script.js.v"
    ['f', 'u', 'n', 'c', 't', 'i', 'o', 'n', ' ', 'f', '(', 'x', ')', ' ', '{', ' ', 'r', 'e', 't', 'u', 'r', 'n', ' ', A<['x', '*', 'x'] , ['M', 'a', 't', 'h', '.', 'p', 'o', 'w', '(', 'x', ',', '2', ')']>, ';', ' ', '}', '\n']

This file depends on Text.ParserCombinators.Parsec which can be installed using:

    cabal install parsec

### Running Parsers

The easiest way to run a variational parser is with the `runParser :: VParser a 
-> VString -> Either String a` function. For example, we can connect the above
variational string loading code to the VJS parser with the following. (The 
parser named `block` is the main parser for VJS, it loads a block of code.)

    > vstringFromFile "script.js.v" >>= return . runParser block
    Right (Block [CtrlStruct (FunctionDef "f" (Args ["x"]) (Block [Stmt (ReturnStmt (ExprC 'A' (BinOpExpr Mul (VarExpr (Reference [RefId "x"])) (VarExpr (Reference [RefId "x"]))) (FunctionCall (Reference [RefId "Math", RefId "pow"]) [VarExpr (Reference [RefId "x"]), LitExpr (NumLit "2.")])))]))])

The `Either` result of `runParser` encodes failure cases, so feeding it nonsense
results in a `Left` construct.

    > runParser block $ VL [Elems "bad parse"]
    Left "Failed to consume whole input: ['b', 'a', 'd', ' ', 'p', 'a', 'r', 's', 'e']"

For VLC, the main parser is named `vlcp`.

    > runParser vlcp $ VL [Elems "\\x.\\y.zw"]
    Right (λx.(λy.(zw)))

## Distillation Codebase (distill directory)

This code represents the distillation algorithm and tool. This code was written
long before the VParser code, so it uses a different variational string type. 
This is why it's in a separate directory.

### CCLib.hs

This is another implementation of variational data types and some functions for
using them. This file also contains the `distill` function.

This code depends on Text.ParserCombinators.Parsec, 
Text.ParserCombinators.Parsec.Char and Data.Algorithm.Diff. They can be 
installed with:

    cabal install parsec diff

### CCTrack.hs

This file does the change tracking as described in chapter 4 of my project 
report. It's meant to be compiled as a program.

It assumes that the programmer wants to use the *latest selection* but, if this
is to be used to support nonlinear editing, there needs to be a way to specify
selectors on the command line. Code from CCSelect.hs can be adapted for this and
replace the use of `(latest vtext)` in this file.

For example, to undo a change marked by dimension "D", then it's just a matter 
of running CCTrack like normal, but with the "D" dimension **left** selected
rather than **right** selected.

### CCSelect.hs

This file projcts a variational source file to a regular source file. It's 
intended to be compiled as a program.
