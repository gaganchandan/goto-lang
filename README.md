# goto-lang
`goto-lang` is a simple programming language with only GOTO statements for control flow and a small set of instructions.

## Getting started
```
git clone https://github.com/gaganchandan/goto-lang
cd goto-lang/src
dune build --release
dune exec --release goto <program>
```

# Language features
`goto-lang` only allows operations in two data types, `integers` and `string`. Variables can be defined of either of these two types. 17 different instructions are avaailabe. The general syntax for instructions is as follows:

` INSTR op1 op2 .. opn`
Control flow is performed using the `goto` instruction. Conditional `goto` instruction make use of something known as the `cmpflag` which is partly inspired by flags and conditional jumps in `x86` assembly.

For more information, look at the documentation.


