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
  | Comment
  | Empty
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
let re_comment = Str.regexp "^--.*"
let not_comment str = not (Str.string_match re_comment str 0)

(* Split on lines unless they are wrapped in quotes *)
let lines src =
  let non_empty str = not (String.is_empty str) in
  src |> String.split_lines

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

let tokenize_helper = function
  | "VAR" | "var" -> { token_type = Var; lexeme = "var"; literal = None }
  | "ADD" | "add" -> { token_type = Add; lexeme = "add"; literal = None }
  | "SUB" | "sub" -> { token_type = Sub; lexeme = "sub"; literal = None }
  | "MUL" | "mul" -> { token_type = Mul; lexeme = "mul"; literal = None }
  | "DIV" | "div" -> { token_type = Div; lexeme = "div"; literal = None }
  | "MOD" | "mod" -> { token_type = Mod; lexeme = "mod"; literal = None }
  | "GT" | "gt" -> { token_type = GT; lexeme = "gt"; literal = None }
  | "LT" | "lt" -> { token_type = LT; lexeme = "lt"; literal = None }
  | "EQ" | "eq" -> { token_type = EQ; lexeme = "eq"; literal = None }
  | "NEQ" | "neq" -> { token_type = NEQ; lexeme = "neq"; literal = None }
  | "GE" | "ge" -> { token_type = GE; lexeme = "ge"; literal = None }
  | "LE" | "le" -> { token_type = LE; lexeme = "le"; literal = None }
  | "PRINT" | "print" ->
      { token_type = Print; lexeme = "print"; literal = None }
  | "GETSTR" | "getstr" ->
      { token_type = GetStr; lexeme = "getstr"; literal = None }
  | "GETINT" | "getint" ->
      { token_type = GetInt; lexeme = "getint"; literal = None }
  | "GOTO" | "goto" -> { token_type = Goto; lexeme = "goto"; literal = None }
  | "EXIT" | "exit" -> { token_type = Exit; lexeme = "exit"; literal = None }
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

let tokenize line =
  match line with
  | [] -> [ { token_type = Empty; lexeme = ""; literal = None } ]
  | str when Str.string_match re_comment (List.hd_exn str) 0 ->
      [ { token_type = Comment; lexeme = ""; literal = None } ]
  | str -> List.map ~f:tokenize_helper str

let lex src =
  src |> clean |> Array.map ~f:tokenize |> Array.to_list |> line_num_helper 1 []
  |> List.rev |> Array.of_list
