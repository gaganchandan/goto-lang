open Core

type token_type =
  | Int
  | Str
  | Var
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Print
  | GetStr
  | GetInt
  | Check
  | Goto
  | Invalid

type literal_type = Int of int | Str of string

type token = {
  token_type : token_type;
  lexeme : string;
  literal : string option;
}

type stmt = token list

let re_var = Str.regexp "[a-zA-z_][a-zA-z0-9_]*"
let re_num = Str.regexp "0-9"
let re_str = Str.regexp "\"[^\"]*\""

let keywords =
  [
    "var";
    "add";
    "sub";
    "mul";
    "div";
    "mod";
    "print";
    "getstr";
    "getint";
    "check";
    "goto";
  ]

(* Split on lines unless they are wrapped in quotes *)
let lines src =
  let non_empty str = not (String.is_empty str) in
  src |> String.split_lines |> List.filter ~f:non_empty

(* Split on whitespaces unless they are wrapped in quotes *)
let split_on_whitespace str =
  let re = Str.regexp "[^ \t\n\r\"]+\\|\"[^\"]*\"" in
  Str.full_split re str
  |> List.filter_map ~f:(function Str.Delim s -> Some s | Str.Text _ -> None)

let clean src = src |> lines |> List.map ~f:split_on_whitespace

(* Remove starting and ending qoutes from string literals *)
let clean_str str =
  let len = String.length str in
  String.sub str ~pos:1 ~len:(len - 2)

(* Convert escaped characters to their actual values *)
let unescape str =
  let re = Str.regexp "\\\\n" in
  Str.global_replace re "\n" str

let tokenize = function
  | "var" -> { token_type = Var; lexeme = "var"; literal = None }
  | "add" -> { token_type = Add; lexeme = "add"; literal = None }
  | "sub" -> { token_type = Sub; lexeme = "sub"; literal = None }
  | "mul" -> { token_type = Mul; lexeme = "mul"; literal = None }
  | "div" -> { token_type = Div; lexeme = "div"; literal = None }
  | "mod" -> { token_type = Mod; lexeme = "mod"; literal = None }
  | "print" -> { token_type = Print; lexeme = "print"; literal = None }
  | "getstr" -> { token_type = GetStr; lexeme = "getstr"; literal = None }
  | "getint" -> { token_type = GetInt; lexeme = "getint"; literal = None }
  | "check" -> { token_type = Check; lexeme = "check"; literal = None }
  | "goto" -> { token_type = Goto; lexeme = "goto"; literal = None }
  | str when Str.string_match re_str str 0 ->
      {
        token_type = Str;
        lexeme = str;
        literal = Some (str |> clean_str |> unescape);
      }
  | id when Str.string_match re_var id 0 ->
      { token_type = Str; lexeme = id; literal = Some id }
  | num when Str.string_match re_num num 0 ->
      { token_type = Int; lexeme = num; literal = Some num }
  | invalid -> { token_type = Invalid; lexeme = invalid; literal = None }

let lex = List.map ~f:tokenize
