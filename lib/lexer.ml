open Core

type token_type =
  | Int
  | Str
  | Id
  | Var
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | GT
  | LT
  | EQ
  | NEQ
  | GE
  | LE
  | Print
  | GetStr
  | GetInt
  | Goto
  | Exit
  | Invalid

type literal_type = Int of int | Str of string

let int_literal_to_int = function
  | Int num -> num
  | _ ->
      print_endline "Invalid operand";
      exit (-1)

let str_literal_to_str = function
  | Str str -> str
  | _ ->
      print_endline "Invalid operand";
      exit (-1)

type token = {
  token_type : token_type;
  lexeme : string;
  literal : literal_type option;
}

let re_var = Str.regexp "^[a-zA-z_][a-zA-z0-9_]*"
let re_num = Str.regexp "^[-]?^[0-9]+"
let re_str = Str.regexp "\"[^\"]*\""

(* Split on lines unless they are wrapped in quotes *)
let lines src =
  let non_empty str = not (String.is_empty str) in
  src |> String.split_lines |> List.filter ~f:non_empty

(* Split on whitespaces unless they are wrapped in quotes *)
let split_on_whitespace str =
  let re = Str.regexp "[^ \t\n\r\"]+\\|\"[^\"]*\"" in
  Str.full_split re str
  |> List.filter_map ~f:(function Str.Delim s -> Some s | Str.Text _ -> None)

let clean src =
  src |> lines |> Array.of_list |> Array.map ~f:split_on_whitespace

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
  | "gt" -> { token_type = GT; lexeme = "gt"; literal = None }
  | "lt" -> { token_type = LT; lexeme = "lt"; literal = None }
  | "eq" -> { token_type = EQ; lexeme = "eq"; literal = None }
  | "ge" -> { token_type = GE; lexeme = "ge"; literal = None }
  | "le" -> { token_type = LE; lexeme = "le"; literal = None }
  | "print" -> { token_type = Print; lexeme = "print"; literal = None }
  | "getstr" -> { token_type = GetStr; lexeme = "getstr"; literal = None }
  | "getint" -> { token_type = GetInt; lexeme = "getint"; literal = None }
  | "goto" -> { token_type = Goto; lexeme = "goto"; literal = None }
  | "exit" -> { token_type = Exit; lexeme = "exit"; literal = None }
  | str
    when Str.string_match re_str str 0 && Str.match_end () = String.length str
    ->
      {
        token_type = Str;
        lexeme = str;
        literal = Some (Str (str |> clean_str |> unescape));
      }
  | id when Str.string_match re_var id 0 && Str.match_end () = String.length id
    ->
      { token_type = Id; lexeme = id; literal = None }
  | num
    when Str.string_match re_num num 0 && Str.match_end () = String.length num
    ->
      {
        token_type = Int;
        lexeme = num;
        literal = Some (Int (Int.of_string num));
      }
  | invalid -> { token_type = Invalid; lexeme = invalid; literal = None }

let rec line_num_helper num res lines =
  match lines with
  | [] -> res
  | line :: rest -> line_num_helper (num + 1) ((line, num) :: res) rest

let lex src =
  src |> clean
  |> Array.map ~f:(List.map ~f:tokenize)
  |> Array.to_list |> line_num_helper 1 [] |> List.rev |> Array.of_list
