(** Top Level Environment *)

module U = Unix
module A = Ast

type serve_exit_state =
  | Serve_success
  | Serve_error

let serve_exit_code = function
  | Serve_success -> 0
  | Serve_error -> 1

type cmd_line_args = string list

let parse_cmd_line_args (): cmd_line_args =
  let argv = Sys.argv in
  if Array.length argv < 2
  then
    ( print_endline "expected at least one argument";
      raise Error_msg.Error )
  else List.tl (Array.to_list argv)

let rec load (raw : Ast.env) (filenames : string list) : Ast.env =
  match filenames with
  | (file::filenames) -> load (raw @ Parse.parse file) filenames
  | [] -> raw

let main () =
  try
    let env = load [] (parse_cmd_line_args ()) in
    let () = print_string (A.Print.pp_env env) in
    serve_exit_code Serve_success |> Stdlib.exit
  with
  | Error_msg.Error ->
    print_endline "error";
    serve_exit_code Serve_error |> Stdlib.exit
  | Invalid_argument e ->
    print_endline e;
    print_endline "error";
    serve_exit_code Serve_error |> Stdlib.exit
