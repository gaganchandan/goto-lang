open Core
open Lexer

type rvalue = Int of int | Str of string | Id of string
type lvalue = Id of string
type line = Id of string | Int of int

type stmt =
  | Var of lvalue * rvalue
  | Add of lvalue * rvalue * rvalue
  | Sub of lvalue * rvalue * rvalue
  | Mul of lvalue * rvalue * rvalue
  | Div of lvalue * rvalue * rvalue
  | Mod of lvalue * rvalue * rvalue
  | GT of rvalue * rvalue
  | LT of rvalue * rvalue
  | EQ of rvalue * rvalue
  | GTE of rvalue * rvalue
  | LTE of rvalue * rvalue
  | Print of rvalue
  | GetStr of lvalue
  | GetInt of lvalue
  | Goto of line
  | Error of string

type prog = stmt array
