" Vim syntax file for the goto-lang

syn keyword Keyword var VAR add ADD addi ADDI inc INC sub SUB subi SUBI dec DEC mul MUL muli MULI div DIV divi DIVI MOD mod modi MODI GT gt LT lt eq EQ neqNEQ ge GE le LE print PRINT getstr GETSTR getint GETINT goto GOTO exit EXIT 
syn match Comment "^--.*"
syn match String "\".*\""
syn match Number "\<[-]\=\d\+\>"

hi def link Keyword Statement
hi def link Comment Comment
hi def link String String 
hi def link Number Number

