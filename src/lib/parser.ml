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

let token_to_label (token : token) : label =
  match token with
  | { token_type = Label; literal = Some (Lab s) } -> Lab s
  | _ ->
      print_endline
        "Not a label. This should not happen. Consider filing a bug report.";
      exit (-1)

let token_to_labelname (token : token) : labelname =
  match token with
  | { token_type = LabelName; literal = Some (Lab s) } -> LabName s
  | _ ->
      print_endline
        "Not a label name. This should not happen. Consider filing a bug \
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

let is_int (token : token) : bool =
  match token with { token_type = Int } -> true | _ -> false

let is_label (token : token) : bool =
  match token with { token_type = Label } -> true | _ -> false

let is_labelname (token : token) : bool =
  match token with { token_type = LabelName } -> true | _ -> false

let[@warning "-8"] parse_var operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the var instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the var instruction."
  | [ id; value ] ->
      if is_lvalue id then
        if is_rvalue value then Var (token_to_lvalue id, token_to_rvalue value)
        else parse_error num (value.lexeme ^ " is not a valid value.")
      else parse_error num (id.lexeme ^ " is not a valid variable.")

let[@warning "-8"] parse_add operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the add instruction."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the add instruction."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Add (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (op2.lexeme ^ " is not a valid operand for the add instruction.")
        else
          parse_error num
            (op1.lexeme ^ " is not a valid operand for the add instruction.")
      else
        parse_error num
          (dest.lexeme
         ^ " is not a valid destination for the add instruction. Destination \
            must be a variable.")

let[@warning "-8"] parse_addi operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the addi instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the addi instruction."
  | [ dest; op ] -> parse_add [ dest; dest; op ] num

let[@warning "-8"] parse_inc operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the inc instruction."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the inc instruction."
  | [ dest ] ->
      parse_add
        [
          dest; dest; { token_type = Int; lexeme = "1"; literal = Some (Int 1) };
        ]
        num

let[@warning "-8"] parse_sub operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the sub instruction."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the sub instruction."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Sub (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (op2.lexeme ^ " is not a valid operand for the sub instruction.")
        else
          parse_error num
            (op1.lexeme ^ " is not a valid operand for the sub instruction.")
      else
        parse_error num
          (dest.lexeme
         ^ " is not a valid destination for the sub instruction. Destination \
            must be a variable.")

let[@warning "-8"] parse_subi operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the subi instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the subi instruction."
  | [ dest; op ] -> parse_sub [ dest; dest; op ] num

let[@warning "-8"] parse_dec operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the dec instruction."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the dec instruction."
  | [ dest ] ->
      parse_sub
        [
          dest; dest; { token_type = Int; lexeme = "1"; literal = Some (Int 1) };
        ]
        num

let[@warning "-8"] parse_mul operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the mul instruction."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the mul instruction."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Mul (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (op2.lexeme ^ " is not a valid operand for the mul instruction.")
        else
          parse_error num
            (op1.lexeme ^ " is not a valid operand for the mul instruction.")
      else
        parse_error num
          (dest.lexeme
         ^ " is not a valid destination for the mul instruction. Destination \
            must be a variable.")

let[@warning "-8"] parse_muli operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the muli instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the muli instruction."
  | [ dest; op ] -> parse_mul [ dest; dest; op ] num

let[@warning "-8"] parse_div operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the div instruction."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the div instruction."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Div (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (op2.lexeme ^ " is not a valid operand for the div instruction.")
        else
          parse_error num
            (op1.lexeme ^ " is not a valid operand for the div instruction.")
      else
        parse_error num
          (dest.lexeme
         ^ " is not a valid destination for the div instruction. Destination \
            must be a variable.")

let[@warning "-8"] parse_divi operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the divi instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the divi instruction."
  | [ dest; op ] -> parse_div [ dest; dest; op ] num

let[@warning "-8"] parse_mod operands num =
  match operands with
  | _ when List.length operands < 3 ->
      parse_error num "Too few operands for the mod instruction."
  | _ when List.length operands > 3 ->
      parse_error num "Too many operands for the mod instruction."
  | [ dest; op1; op2 ] ->
      if is_lvalue dest then
        if is_rvalue op1 then
          if is_rvalue op2 then
            Mod (token_to_lvalue dest, token_to_rvalue op1, token_to_rvalue op2)
          else
            parse_error num
              (op2.lexeme ^ " is not a valid operand for the mod instruction.")
        else
          parse_error num
            (op1.lexeme ^ " is not a valid operand for the mod instruction.")
      else
        parse_error num
          (dest.lexeme
         ^ " is not a valid destination for the mod instruction. Destination \
            must be a variable.")

let[@warning "-8"] parse_modi operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the modi instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the modi instruction."
  | [ dest; op ] -> parse_mod [ dest; dest; op ] num

let[@warning "-8"] parse_gt operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the gt instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the gt instruction."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then GT (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (op2.lexeme
           ^ " is not a valid operand for the gt instruction. Operand must be \
              an integer literal, string literal, or a variable.")
      else
        parse_error num
          (op1.lexeme
         ^ " is not a valid operand for the gt instruction. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_lt operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num
        "Too few operands for the lt instruction. Operand must be an integer \
         literal, string literal, or a variable."
  | _ when List.length operands > 2 ->
      parse_error num
        "Too many operands for the lt instruction. Operand must be an integer \
         literal, string literal, or a variable."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then LT (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (op2.lexeme
           ^ " is not a valid operand for the lt instruction. Operand must be \
              an integer literal, string literal, or a variable.")
      else
        parse_error num
          (op1.lexeme
         ^ " is not a valid operand for the lt instruction. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_eq operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the eq instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the eq instruction."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then EQ (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (op2.lexeme
           ^ " is not a valid operand for the eq instruction. Operand must be \
              an integer literal, string literal, or a variable.")
      else
        parse_error num
          (op1.lexeme
         ^ " is not a valid operand for the eq instruction. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_neq operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the neq instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the neq instruction."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then NEQ (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (op2.lexeme
           ^ " is not a valid operand for the neq instruction. Operand must be \
              an integer literal, string literal, or a variable.")
      else
        parse_error num
          (op1.lexeme
         ^ " is not a valid operand for the eq instruction. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_ge operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the ge instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the ge instruction."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then GE (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (op2.lexeme
           ^ " is not a valid operand for the ge instruction. Operand must be \
              an integer literal, string literal, or a variable.")
      else
        parse_error num
          (op1.lexeme
         ^ " is not a valid operand for the ge instruction. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_le operands num =
  match operands with
  | _ when List.length operands < 2 ->
      parse_error num "Too few operands for the le instruction."
  | _ when List.length operands > 2 ->
      parse_error num "Too many operands for the le instruction."
  | [ op1; op2 ] ->
      if is_rvalue op1 then
        if is_rvalue op2 then LE (token_to_rvalue op1, token_to_rvalue op2)
        else
          parse_error num
            (op2.lexeme
           ^ " is not a valid operand for the le instruction. Operand must be \
              an integer literal, string literal, or a variable.")
      else
        parse_error num
          (op1.lexeme
         ^ " is not a valid operand for the le instruction. Operand must be an \
            integer literal, string literal, or a variable.")

let[@warning "-8"] parse_print operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the print instruction."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the print instruction."
  | [ op ] ->
      if is_rvalue op then Print (token_to_rvalue op)
      else
        parse_error num
          (op.lexeme
         ^ " is not a valid operand for the print instruction. Operand must be \
            a integer literal, string literal or a variable.")

let[@warning "-8"] parse_getstr operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the getstr instruction."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the getstr instruction."
  | [ dest ] ->
      if is_lvalue dest then GetStr (token_to_lvalue dest)
      else
        parse_error num
          (dest.lexeme
         ^ " is not a valid operand for the getstr instruction. Operand must \
            be a variable.")

let[@warning "-8"] parse_getint operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the getint instruction."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the getint instruction."
  | [ dest ] ->
      if is_lvalue dest then GetInt (token_to_lvalue dest)
      else
        parse_error num
          (dest.lexeme
         ^ " is not a valid operand for the getint instruction. Operand must \
            be a variable.")

let[@warning "-8"] parse_goto operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the goto instruction."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the goto instruction."
  | [ labelname ] ->
      if is_labelname labelname then Goto (token_to_labelname labelname)
      else
        parse_error num
          (labelname.lexeme
         ^ " is not a valid operand for the goto instruction. Operand must be \
            a valid label name")

let[@warning "-8"] parse_exit operands num =
  match operands with
  | _ when List.length operands < 1 ->
      parse_error num "Too few operands for the exit instruction."
  | _ when List.length operands > 1 ->
      parse_error num "Too many operands for the exit instruction."
  | [ op ] ->
      if is_int op then Exit (op.lexeme |> int_of_string)
      else
        parse_error num
          (op.lexeme
         ^ " is not a valid operand for the exit instruction. Operand must be \
            an integer indicating the exit code")

let parse_op (instruction : token) (operands : token list) (num : int) : stmt =
  match instruction with
  | { token_type = Int } ->
      parse_error num "Lines must start with an instruction or label."
  | { token_type = Str } ->
      parse_error num "Lines must start with an instruction or label."
  | { token_type = Id } ->
      parse_error num "Lines must start with an instruction or label."
  | { token_type = LabelName } ->
      parse_error num "Lines must start with an instruction or label."
  | { token_type = Var } -> parse_var operands num
  | { token_type = Add } -> parse_add operands num
  | { token_type = Addi } -> parse_addi operands num
  | { token_type = Inc } -> parse_inc operands num
  | { token_type = Sub } -> parse_sub operands num
  | { token_type = Subi } -> parse_subi operands num
  | { token_type = Dec } -> parse_dec operands num
  | { token_type = Mul } -> parse_mul operands num
  | { token_type = Muli } -> parse_muli operands num
  | { token_type = Div } -> parse_div operands num
  | { token_type = Divi } -> parse_divi operands num
  | { token_type = Mod } -> parse_mod operands num
  | { token_type = Modi } -> parse_modi operands num
  | { token_type = GT } -> parse_gt operands num
  | { token_type = LT } -> parse_lt operands num
  | { token_type = EQ } -> parse_eq operands num
  | { token_type = NEQ } -> parse_neq operands num
  | { token_type = GE } -> parse_ge operands num
  | { token_type = LE } -> parse_le operands num
  | { token_type = Print } -> parse_print operands num
  | { token_type = GetStr } -> parse_getstr operands num
  | { token_type = GetInt } -> parse_getint operands num
  | { token_type = Goto } -> parse_goto operands num
  | { token_type = Label } -> Label (token_to_label instruction)
  | { token_type = Exit } -> parse_exit operands num
  | { token_type = Comment } -> Comment
  | { token_type = Empty } -> Empty
  | { token_type = Invalid; lexeme } ->
      parse_error num ("Invalid token " ^ lexeme)

let parse_line line =
  match line with
  | [], num ->
      parse_error num
        "Empty line encountered while parsing. This should not happen. \
         Consider filing a bug report."
  | instruction :: operands, num -> parse_op instruction operands num

let parse lines : prog = Array.map ~f:parse_line lines
