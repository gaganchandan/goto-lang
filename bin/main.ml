open Core

let () =
  let args = Array.to_list Sys.argv in
  if List.length args != 2 then (
    print_endline "Usage: gotoc <program>";
    exit 1)

let args = Array.to_list Sys.argv
let file = List.nth_exn args 1
