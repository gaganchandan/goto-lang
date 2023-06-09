open Core
open Goto.Lexer
open Goto.Parser
open Goto.Eval

let () =
  let args = Array.to_list Sys.argv in
  if List.length args != 2 then (
    print_endline "Usage: goto <program>";
    exit 1)

let args = Array.to_list Sys.argv
let file = List.nth_exn args 1

let src =
  try In_channel.read_all file
  with e ->
    print_endline ("Unable to read from file " ^ file);
    exit (-1)

let () = src |> lex |> parse |> eval
