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
    let open Core in
    let args = parse_cmd_line_args () in
    let env = load [] args in
(*  let () = print_string (A.Print.pp_env env) in *)
    let out_channel = open_out ((List.hd_exn args) ^ ".val") in
    let print_string s = output_string out_channel s in
    let type_names = env |> List.filter_map ~f:(function A.TypeDefn (n, d) -> Some (n, d) | _ -> None) |> Map.of_alist_exn (module String) in
    let procs = env |> List.filter_map ~f:(function A.ProcDefn (n, d, a, b) -> Some (n, (d, a, b)) | _ -> None) |> Map.of_alist_exn (module String) in
    let compile_env = Compiler.{ type_names; procs } in
    let (compile_dest, asm) = Compiler.compiler compile_env in
    List.iter env ~f:(function
        | A.ProcDefn (n, d, a, b) -> 
                Printf.printf "proc %s:\n" n;
                let vars = Map.of_alist_exn (module String) a in
                let (insts, e) = compile_dest ~cmd:b ~dest:d ~vars Compiler.empty_func_env in
                List.iter insts ~f:Compiler.print_macro_inst;
                Compiler.print_func_env e;
                let st =
                    let locals = List.map a ~f:(fun (v, _t) -> Compiler.Addr v) in
                    let stack = [] in
                    let addrs = Map.of_alist_exn (module String) @@ List.map a ~f:(fun (v, _t) -> (v, [])) in
                    let free_locals = [] in
                    Compiler.{ stack ; locals; addrs; free_locals }
                in
                let wasm = asm insts st e in
                Printf.printf "WASM:\n";
                List.iter wasm ~f:(fun i -> Wasm.Print.instr Out_channel.stdout 120 @@ Compiler.to_region i);
                Printf.printf "\n";
                ()
        | _ -> ()
    );
    (*let _ = eval_and_print print_string env in*)
    (*let _ = close_out out_channel in*)
    serve_exit_code Serve_success |> Stdlib.exit
  with
  | Error_msg.Error ->
    print_endline "error";
    serve_exit_code Serve_error |> Stdlib.exit
  | Invalid_argument e ->
    print_endline e;
    print_endline "error";
    serve_exit_code Serve_error |> Stdlib.exit
  | exn -> 
          Printexc.print_backtrace stdout;
          print_endline ((List.hd (parse_cmd_line_args ())) ^ ": exception: " ^ Printexc.to_string exn)
