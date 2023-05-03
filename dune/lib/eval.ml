open Core
open Lexer
open Syntax
open Parser

type binding = { name : string; data : data }
type env = binding list
type context = { env : env; cmpflag : int; cur : int }

let eval_error num msg =
  print_endline ("EvalError on line " ^ string_of_int num ^ ": " ^ msg);
  exit (-1)

let type_error num msg =
  print_endline ("TypeError on line " ^ string_of_int num ^ ": " ^ msg);
  exit (-1)

let env_match (name : string) (binding : binding) : bool =
  if String.equal binding.name name then true else false

let env_match_n (name : string) (binding : binding) : bool =
  if String.equal binding.name name then false else true

let env_insert (binding : binding) (env : env) : env =
  let filtered = List.filter ~f:(env_match_n binding.name) env in
  match binding with { name; data } -> binding :: filtered

let env_fetch (name : string) (env : env) (line : int) : data =
  let filtered = List.filter ~f:(env_match name) env in
  match filtered with
  | [] -> eval_error line ("Variable " ^ name ^ " is not defined.")
  | binding :: _ -> binding.data

let rec update_env (update : env) (env : env) : env =
  match update with
  | [] -> env
  | binding :: bindings -> update_env bindings (env_insert binding env)

let resolve (rvalue : rvalue) (env : env) (line : int) : data =
  match rvalue with
  | Int n -> Int n
  | Str s -> Str s
  | Id i -> env_fetch i env (line + 1)

let resolve_line (line : line) (env : env) (cur : int) : int =
  match line with
  | Int n -> n - 1
  | Id i -> (
      let value = env_fetch i env cur in
      match value with
      | Int n -> n - 1
      | Str s ->
          eval_error (cur + 1)
            "A string cannot be passed to the goto instruction.")

let[@warning "-8"] eval_var (stmt : stmt) (context : context) : context =
  match stmt with
  | Var (Id id, rvalue) ->
      {
        env =
          update_env
            [ { name = id; data = resolve rvalue context.env context.cur } ]
            context.env;
        cmpflag = 1;
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_add (stmt : stmt) (context : context) : context =
  let add (d1 : data) (d2 : data) (line : int) : data =
    match (d1, d2) with
    | Int n1, Int n2 -> Int (n1 + n2)
    | Str s1, Str s2 -> Str (s1 ^ s2)
    | _ -> type_error line "Operand type mismatch for the add instruction."
  in
  match stmt with
  | Add (Id dest, d1, d2) ->
      {
        env =
          update_env
            [
              {
                name = dest;
                data =
                  add
                    (resolve d1 context.env context.cur)
                    (resolve d2 context.env context.cur)
                    (context.cur + 1);
              };
            ]
            context.env;
        cmpflag = 1;
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_sub (stmt : stmt) (context : context) : context =
  let sub (d1 : data) (d2 : data) (line : int) : data =
    match (d1, d2) with
    | Int n1, Int n2 -> Int (n1 - n2)
    | _ -> type_error line "Operand type mismatch for the sub instruction."
  in
  match stmt with
  | Sub (Id dest, d1, d2) ->
      {
        env =
          update_env
            [
              {
                name = dest;
                data =
                  sub
                    (resolve d1 context.env context.cur)
                    (resolve d2 context.env context.cur)
                    (context.cur + 1);
              };
            ]
            context.env;
        cmpflag = 1;
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_mul (stmt : stmt) (context : context) : context =
  let mul (d1 : data) (d2 : data) (line : int) : data =
    match (d1, d2) with
    | Int n1, Int n2 -> Int (n1 * n2)
    | _ -> type_error line "Operand type mismatch for the mul instruction."
  in
  match stmt with
  | Mul (Id dest, d1, d2) ->
      {
        env =
          update_env
            [
              {
                name = dest;
                data =
                  mul
                    (resolve d1 context.env context.cur)
                    (resolve d2 context.env context.cur)
                    (context.cur + 1);
              };
            ]
            context.env;
        cmpflag = 1;
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_div (stmt : stmt) (context : context) : context =
  let div (d1 : data) (d2 : data) (line : int) : data =
    match (d1, d2) with
    | Int n1, Int n2 -> Int (n1 / n2)
    | _ -> type_error line "Operand type mismatch for the div instruction."
  in
  match stmt with
  | Div (Id dest, d1, d2) ->
      {
        env =
          update_env
            [
              {
                name = dest;
                data =
                  div
                    (resolve d1 context.env context.cur)
                    (resolve d2 context.env context.cur)
                    (context.cur + 1);
              };
            ]
            context.env;
        cmpflag = 1;
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_mod (stmt : stmt) (context : context) : context =
  let modulus (d1 : data) (d2 : data) (line : int) : data =
    match (d1, d2) with
    | Int n1, Int n2 -> Int (n1 mod n2)
    | _ -> type_error line "Operand type mismatch for the mod instruction."
  in
  match stmt with
  | Mod (Id dest, d1, d2) ->
      {
        env =
          update_env
            [
              {
                name = dest;
                data =
                  modulus
                    (resolve d1 context.env context.cur)
                    (resolve d2 context.env context.cur)
                    (context.cur + 1);
              };
            ]
            context.env;
        cmpflag = 1;
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_gt (stmt : stmt) (context : context) : context =
  let gt (d1 : data) (d2 : data) (line : int) : int =
    match (d1, d2) with
    | Int n1, Int n2 -> if n1 > n2 then 1 else 0
    | _ -> type_error line "Operand type mismatch for the gt instruction."
  in
  match stmt with
  | GT (d1, d2) ->
      {
        env = context.env;
        cmpflag =
          gt
            (resolve d1 context.env context.cur)
            (resolve d2 context.env context.cur)
            (context.cur + 1);
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_lt (stmt : stmt) (context : context) : context =
  let lt (d1 : data) (d2 : data) (line : int) : int =
    match (d1, d2) with
    | Int n1, Int n2 -> if n1 < n2 then 1 else 0
    | _ -> type_error line "Operand type mismatch for the lt instruction."
  in
  match stmt with
  | LT (d1, d2) ->
      {
        env = context.env;
        cmpflag =
          lt
            (resolve d1 context.env context.cur)
            (resolve d2 context.env context.cur)
            (context.cur + 1);
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_eq (stmt : stmt) (context : context) : context =
  let eq (d1 : data) (d2 : data) (line : int) : int =
    match (d1, d2) with
    | Int n1, Int n2 -> if n1 = n2 then 1 else 0
    | Str s1, Str s2 -> if String.equal s1 s2 then 1 else 0
    | _ -> type_error line "Operand type mismatch for the eq instruction."
  in
  match stmt with
  | EQ (d1, d2) ->
      {
        env = context.env;
        cmpflag =
          eq
            (resolve d1 context.env context.cur)
            (resolve d2 context.env context.cur)
            (context.cur + 1);
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_neq (stmt : stmt) (context : context) : context =
  let neq (d1 : data) (d2 : data) (line : int) : int =
    match (d1, d2) with
    | Int n1, Int n2 -> if n1 = n2 then 0 else 1
    | Str s1, Str s2 -> if String.equal s1 s2 then 0 else 1
    | _ -> type_error line "Operand type mismatch for the neq instruction."
  in
  match stmt with
  | NEQ (d1, d2) ->
      {
        env = context.env;
        cmpflag =
          neq
            (resolve d1 context.env context.cur)
            (resolve d2 context.env context.cur)
            (context.cur + 1);
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_ge (stmt : stmt) (context : context) : context =
  let ge (d1 : data) (d2 : data) (line : int) : int =
    match (d1, d2) with
    | Int n1, Int n2 -> if n1 >= n2 then 1 else 0
    | _ -> type_error line "Operand type mismatch for the ge instruction."
  in
  match stmt with
  | GE (d1, d2) ->
      {
        env = context.env;
        cmpflag =
          ge
            (resolve d1 context.env context.cur)
            (resolve d2 context.env context.cur)
            (context.cur + 1);
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_le (stmt : stmt) (context : context) : context =
  let le (d1 : data) (d2 : data) (line : int) : int =
    match (d1, d2) with
    | Int n1, Int n2 -> if n1 <= n2 then 1 else 0
    | _ -> type_error line "Operand type mismatch for the le instruction."
  in
  match stmt with
  | LE (d1, d2) ->
      {
        env = context.env;
        cmpflag =
          le
            (resolve d1 context.env context.cur)
            (resolve d2 context.env context.cur)
            (context.cur + 1);
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_print (stmt : stmt) (context : context) : context =
  let print (d : data) : unit =
    match d with Int n -> print_int n | Str s -> print_string s
  in
  match stmt with
  | Print value ->
      let () = print (resolve value context.env context.cur) in
      { env = context.env; cmpflag = 1; cur = context.cur + 1 }

let[@warning "-8"] eval_getstr (stmt : stmt) (context : context) : context =
  let getstr line =
    try Str (read_line ())
    with e -> eval_error line "Invalid input for getstr instruction."
  in
  match stmt with
  | GetStr (Id id) ->
      {
        env =
          update_env
            [ { name = id; data = getstr (context.cur + 1) } ]
            context.env;
        cmpflag = 1;
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_getint (stmt : stmt) (context : context) : context =
  let getint line =
    try Int (read_int ())
    with e -> eval_error line "Invalid input for getint instruction."
  in
  match stmt with
  | GetInt (Id id) ->
      {
        env =
          update_env
            [ { name = id; data = getint (context.cur + 1) } ]
            context.env;
        cmpflag = 1;
        cur = context.cur + 1;
      }

let[@warning "-8"] eval_goto (stmt : stmt) (context : context) : context =
  match stmt with
  | Goto line ->
      if context.cmpflag = 0 then
        { env = context.env; cmpflag = 1; cur = context.cur + 1 }
      else if context.cmpflag = 1 then
        {
          env = context.env;
          cmpflag = 1;
          cur = resolve_line line context.env context.cur;
        }
      else (
        print_endline
          "Invalid value of CmpFlag. This should not happen. Consider filing a \
           bug report.";
        exit (-1))

let[@warning "-8"] eval_exit (stmt : stmt) _ =
  match stmt with Exit code -> exit code

let eval_stmt (stmt : stmt) (context : context) : context =
  match stmt with
  | Var _ -> eval_var stmt context
  | Add _ -> eval_add stmt context
  | Sub _ -> eval_sub stmt context
  | Mul _ -> eval_mul stmt context
  | Div _ -> eval_div stmt context
  | Mod _ -> eval_mod stmt context
  | GT _ -> eval_gt stmt context
  | LT _ -> eval_lt stmt context
  | EQ _ -> eval_eq stmt context
  | NEQ _ -> eval_neq stmt context
  | GE _ -> eval_ge stmt context
  | LE _ -> eval_le stmt context
  | Print _ -> eval_print stmt context
  | GetStr _ -> eval_getstr stmt context
  | GetInt _ -> eval_getint stmt context
  | Goto _ -> eval_goto stmt context
  | Exit _ -> eval_exit stmt context

let rec eval_helper (context : context) (prog : prog) : unit =
  let len = Array.length prog in
  if context.cur = len then exit 0
  else if context.cur > -1 && context.cur < len then
    let stmt = prog.(context.cur) in
    eval_helper (eval_stmt stmt context) prog
  else eval_error (context.cur + 1) "Invalid line number"

let eval = eval_helper { env = []; cmpflag = 1; cur = 0 }
