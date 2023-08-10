# GOTO-LANG 

`goto-lang`, or simply `goto`, is a simple language consisting of 17 different instructions. There is only on instruction used for switching control flow which is the `goto` instruction and hence the name. 


### Syntax

The general syntax for statements in `goto` is as follows:


`INSTRUCTION op1 op2 ... opn`


In plain words, all statements consist of an instruction followed by it's operands. Every statement must start with an instruction. Instructions can only be fully lowercase or fully uppercase.


### Datatypes

There are only 2 datatypes in `goto`, namely `strings` and `integers`. Strings are enclosed within double qoutes only. Negative integers can also be used.


### Identifiers and Labels

`goto` does not contain function calls, and hence control flow is carried out with the use of jumps to 'labels'. The syntax for labels is as follows:


```
LABELNAME:
    stmt1
    stmt2
    ...
```


Label names must begin with an underscore and can be followed by any number of alphanumeric characters or underscores. When defining a label, the label name must be followed by a colon. While jumping to a label, only the label name is used as seen in the syntax for the `goto` statement later on  in this document.


Identifiers/variables must start with an alphabet(lowercase or uppercase) and can be followed any number of alphanumeric characters or underscores. It is important to note that they cannot begin with underscores in order to differentiate them from labels.


### Comments and Whitespaces

Comments begin with `--`. Only single line comments are supported.


Whitespaces are used to seperate tokens. They have no other significance.


### Instructions 

A detailed description of the available instructions follows.


##### VAR

The `VAR` instruction is used to create variables. The syntax is as follows:


`VAR <variable_name> <value>`


Both integer and string values are assigned using the same constant. Variables are also redefined using the same syntax. There is no strong notion of typing when it comes to variables, a variable holding an integer variable can be freely modified to hold a string and vice-versa.


#####


