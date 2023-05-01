open Core
open Lexer
open Syntax

let parse_error num msg =
  print_endline ("ParseError on line " ^ string_of_int num ^ ": " ^ msg);
  exit (-1)

let token_to_rvalue (token : token) : rvalue =
  match token with
  | { token_type = Int; literal = Some literal } ->
      Int (Lexer.int_literal_to_int literal)
  | { token_type = Str; literal = Some literal } ->
      Str (Lexer.str_literal_to_str literal)
  | { token_type = Id; lexeme } -> Id lexeme
  | _ ->
      print_endline
        "Not an rvalue. This should not happen. Consider filing a bug report.";
      exit (-1)

let token_to_lvalue (token : token) : lvalue =
  match token with
  | { token_type = Id; lexeme } -> Id lexeme
  | _ ->
      print_endline
        "Not an lvalue. This should not happen. Consider filing a bug report.";
      exit (-1)

let token_to_line (token : token) : line =
  match token with
  | { token_type = Id; lexeme } -> Id lexeme
  | { token_type = Int; literal = Some literal } ->
      Int (Lexer.int_literal_to_int literal)
  | _ ->
      print_endline
        "Not a valid line. This should not happen. Consider filing a bug \
         report.";
      exit (-1)

let is_rvalue (token : token) : bool =
  match token with
  | { token_type = Int } -> true
  | { token_type = Str } -> true
  | { token_type = Id } -> true
  | _ -> false

let is_lvalue (token : token) : bool =
  match token with { token_type = Id } -> true | _ -> false

let is_line (token : token) : bool =
  match token with
  | { token_type = Int } -> true
  | { token_type = Id } -> true
  | _ -> false

let get_lexeme (token : token) : string =
  match token with { lexeme } -> lexeme

let[@warning "-8"] parse_var operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the var operator."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the var operator."
  | [ id; value ] ->
      if is_lvalue id then
        if is_rvalue value then Var (token_to_lvalue id, token_to_rvalue value)
        else parse_error num (get_lexeme value ^ " is not a valid value.")
      else parse_error num (get_lexeme id ^ " is not a valid variable.")

let[@warning "-8"] parse_add operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the add operator."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the add operator."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Add (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (get_lexeme op2 ^ " is not a valid operand for the add operator.")
        else
          parse_error num
            (get_lexeme op1 ^ " is not a valid operand for the add operator.")
      else
        parse_error num
          (get_lexeme dest
         ^ " is not a valid destination for the add operator. Destination must \
            be a variable.")

let[@warning "-8"] parse_sub operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the sub operator."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the sub operator."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Add (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (get_lexeme op2 ^ " is not a valid operand for the sub operator.")
        else
          parse_error num
            (get_lexeme op1 ^ " is not a valid operand for the sub operator.")
      else
        parse_error num
          (get_lexeme dest
         ^ " is not a valid destination for the sub operator. Destination must \
            be a variable.")

let[@warning "-8"] parse_mul operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the mul operator."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the mul operator."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Add (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (get_lexeme op2 ^ " is not a valid operand for the mul operator.")
        else
          parse_error num
            (get_lexeme op1 ^ " is not a valid operand for the mul operator.")
      else
        parse_error num
          (get_lexeme dest
         ^ " is not a valid destination for the mul operator. Destination must \
            be a variable.")

let[@warning "-8"] parse_div operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the div operator."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the div operator."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Add (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (get_lexeme op2 ^ " is not a valid operand for the div operator.")
        else
          parse_error num
            (get_lexeme op1 ^ " is not a valid operand for the div operator.")
      else
        parse_error num
          (get_lexeme dest
         ^ " is not a valid destination for the div operator. Destination must \
            be a variable.")

let[@warning "-8"] parse_mod operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the mod operator."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the mod operator."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Add (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (get_lexeme op2 ^ " is not a valid operand for the mod operator.")
        else
          parse_error num
            (get_lexeme op1 ^ " is not a valid operand for the mod operator.")
      else
        parse_error num
          (get_lexeme dest
         ^ " is not a valid destination for the mod operator. Destination must \
            be a variable.")

let[@warning "-8"] parse_gt operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the gt operator."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the gt operator."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then GT (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (get_lexeme op2
           ^ " is not a valid operand for the gt operator. Operand must be an \
              integer literal, string literal, or a variable.")
      else
        parse_error num
          (get_lexeme op1
         ^ " is not a valid operand for the gt operator. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_lt operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num
        "Too few operands for the lt operator. Operand must be an integer \
         literal, string literal, or a variable."
  | _ when List.length operands > 2 ->
      parse_error num
        "Too many operands for the lt operator. Operand must be an integer \
         literal, string literal, or a variable."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then LT (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (get_lexeme op2
           ^ " is not a valid operand for the lt operator. Operand must be an \
              integer literal, string literal, or a variable.")
      else
        parse_error num
          (get_lexeme op1
         ^ " is not a valid operand for the lt operator. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_eq operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the eq operator."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the eq operator."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then EQ (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (get_lexeme op2
           ^ " is not a valid operand for the eq operator. Operand must be an \
              integer literal, string literal, or a variable.")
      else
        parse_error num
          (get_lexeme op1
         ^ " is not a valid operand for the eq operator. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_gte operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the gte operator."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the gte operator."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then GTE (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (get_lexeme op2
           ^ " is not a valid operand for the gte operator. Operand must be an \
              integer literal, string literal, or a variable.")
      else
        parse_error num
          (get_lexeme op1
         ^ " is not a valid operand for the gte operator. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_lte operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the lte operator."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the lte operator."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then LTE (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (get_lexeme op2
           ^ " is not a valid operand for the lte operator. Operand must be an \
              integer literal, string literal, or a variable.")
      else
        parse_error num
          (get_lexeme op1
         ^ " is not a valid operand for the lte operator. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_print operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the print operator."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the print operator."
  | [ op ] ->
      if is_rvalue op then Print (token_to_rvalue op)
      else
        parse_error num
          (get_lexeme op
         ^ " is not a valid operand for the print operator. Operand must be a \
            integer literal, string literal or a variable.")

let[@warning "-8"] parse_getstr operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the getstr operator."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the getstr operator."
  | [ dest ] ->
      if is_lvalue dest then GetStr (token_to_lvalue dest)
      else
        parse_error num
          (get_lexeme dest
         ^ " is not a valid operand for the getstr operator. Operand must be a \
            variable.")

let[@warning "-8"] parse_getint operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the getint operator."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the getint operator."
  | [ dest ] ->
      if is_lvalue dest then GetInt (token_to_lvalue dest)
      else
        parse_error num
          (get_lexeme dest
         ^ " is not a valid operand for the getint operator. Operand must be a \
            variable.")

let[@warning "-8"] parse_goto operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the goto operator."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the goto operator."
  | [ dest ] ->
      if is_line dest then Goto (token_to_line dest)
      else
        parse_error num
          (get_lexeme dest
         ^ " is not a valid operand for the goto operator. Operand must be an \
            integer literal or a variable.")

let parse_op (operator : token) (operands : token list) (num : int) : stmt =
  match operator with
  | { token_type = Int } ->
      parse_error num "Statements must start with an operator."
  | { token_type = Str } ->
      parse_error num "Statements must start with an operator."
  | { token_type = Id } ->
      parse_error num "Statements must start with an operator."
  | { token_type = Var } -> (parse_var operands num, num)
  | { token_type = Add } -> (parse_add operands num, num)
  | { token_type = Sub } -> (parse_sub operands num, num)
  | { token_type = Mul } -> (parse_mul operands num, num)
  | { token_type = Div } -> (parse_div operands num, num)
  | { token_type = Mod } -> (parse_mod operands num, num)
  | { token_type = GT } -> (parse_gt operands num, num)
  | { token_type = LT } -> (parse_lt operands num, num)
  | { token_type = EQ } -> (parse_eq operands num, num)
  | { token_type = GTE } -> (parse_gte operands num, num)
  | { token_type = LTE } -> (parse_lte operands num, num)
  | { token_type = Print } -> (parse_print operands num, num)
  | { token_type = GetStr } -> (parse_getstr operands num, num)
  | { token_type = GetInt } -> (parse_getint operands num, num)
  | { token_type = Goto } -> (parse_goto operands num, num)
  | { token_type = Invalid; lexeme } ->
      parse_error num ("Invalid token " ^ lexeme)

let parse_line line =
  match line with
  | [], num ->
      parse_error num
        "Empty line encountered while parsing. This should not happen. \
         Consider filing a bug report."
  | operator :: operands, num -> parse_op operator operands num

let parse = List.map ~f:parse_line
