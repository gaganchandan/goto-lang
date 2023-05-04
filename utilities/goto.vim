syn keyword Keyword var VAR add ADD sub SUB mul MUL div DIV MOD mod GT gt LT lt eq EQ neqNEQ ge GE le LE print PRINT getstr GETSTR getint GETINT goto GOTO exit EXIT 
syn match Comment "^--.*"
syn match String "\".*\""
syn match Number "\<[-]\=\d\+\>"

hi def link Keyword Statement
hi def link Comment Comment
hi def link String String 
hi def link Number Number

