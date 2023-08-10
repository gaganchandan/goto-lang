# goto-lang
`goto-lang` is a simple programming language with only GOTO statements for control flow and a small set of instructions.

## Getting started
```
git clone https://github.com/gaganchandan/goto-lang
cd goto-lang/src
dune build --release
dune exec --release goto <program>
```

## Language features
`goto-lang` only allows operations in two data types, `integers` and `string`. Variables can be defined of either of these two types. 24 different instructions are avaailabe. The general syntax for instructions is as follows:

` INSTR op1 op2 .. opn`

Control flow is performed using the `goto` instruction. Conditional `goto` instruction make use of something known as the `cmpflag` which is partly inspired by flags and conditional jumps in `x86` assembly. Comments are supported as well, and begin with `--` as seen in Lua.

For a detailed description on the various instructions and langauge features, consult the documents in `references/`. The `examples/` directory contains trivial programs showcasing `goto-lang`'s abilities.
If you just want a "Hello, World!" program, then here you go!

```
_main:
  PRINT "Hello, World!\n"
```

A slightly more complex variant:

```
_main:
  PRINT "What is your name?\n"
  GETSTR name 
  VAR msg "Hello, "
  ADDI msg name
  ADDI msg "!\n"
  PRINT msg
```
