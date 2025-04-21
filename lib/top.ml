(** Top Level Environment *)
open Core

module U = Unix
module A = Ast
module W = Wasm.Ast
module WT = Wasm.Types

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
  else List.tl_exn (Array.to_list argv)

let rec load (raw : Ast.env) (filenames : string list) : Ast.env =
  match filenames with
  | (file::filenames) -> load (raw @ Parse.parse file) filenames
  | [] -> raw

let mod_types = [
    WT.(RecT ([SubT (Final, [], DefFuncT (FuncT ([], [])))]));
    WT.(RecT ([SubT (Final, [], DefFuncT (FuncT ([NumT I32T], [])))]));
    WT.(RecT ([SubT (Final, [], DefFuncT (FuncT ([NumT I32T], [NumT I32T])))]));
    WT.(RecT ([SubT (Final, [], DefFuncT (FuncT ([NumT I32T], [NumT I32T; NumT I32T])))]));
    WT.(RecT ([SubT (Final, [], DefFuncT (FuncT ([], [NumT I32T])))]));
    WT.(RecT ([SubT (Final, [], DefFuncT (FuncT ([NumT I32T; NumT I32T], [])))]));
    WT.(RecT ([SubT (Final, [], DefFuncT (FuncT ([NumT I32T; NumT I32T], [NumT I32T])))]));
] |> List.map ~f:Compiler.to_region

let mod_imports = [
    W.{ 
        module_name = Wasm.Utf8.decode "";
        item_name = Wasm.Utf8.decode "mem";
        idesc = W.MemoryImport WT.(MemoryT (I32AT, { min = Int64.of_int_exn 1; max = None })) |> Compiler.to_region 
    };
    W.{
        module_name = Wasm.Utf8.decode "";
        item_name = Wasm.Utf8.decode "alloc";
        idesc = W.FuncImport (Compiler.to_wasm_imm Compiler.type_idxs.pair_to_i32) |> Compiler.to_region (* alloc *)
    };
    W.{
        module_name = Wasm.Utf8.decode "";
        item_name = Wasm.Utf8.decode "free";
        idesc = W.FuncImport (Compiler.to_wasm_imm Compiler.type_idxs.i32_to_unit) |> Compiler.to_region (* free *)
    };
    W.{
        module_name = Wasm.Utf8.decode "";
        item_name = Wasm.Utf8.decode "print_val";
        idesc = W.FuncImport (Compiler.to_wasm_imm Compiler.type_idxs.pair_to_unit) |> Compiler.to_region (* print *)
    }
]

let mk_mod funcs datas = W.{
    W.empty_module with 
    types = mod_types; 
    funcs = funcs |> List.map ~f:Compiler.to_region;
    imports = mod_imports |> List.map ~f:Compiler.to_region;
    exports = [{ name = Wasm.Utf8.decode "serialize_types"; edesc = FuncExport (Compiler.to_wasm_imm 3) |> Compiler.to_region} |> Compiler.to_region];
    datas;
}

let main () =
  try
    let open Core in
    let args = parse_cmd_line_args () in
    let env = load [] args in
(*  let () = print_string (A.Print.pp_env env) in *)
    let out_channel = open_out ((List.hd_exn args) ^ ".val") in
    let print_string s = output_string out_channel s in
    let type_ls = env |> List.filter_map ~f:(function A.TypeDefn (n, d) -> Some (n, d) | _ -> None) in
    let type_names = type_ls |> Map.of_alist_exn (module String) in
    let proc_ls = env |> List.filter_map ~f:(function A.ProcDefn (n, d, a, b) -> Some (n, (d, a, b)) | _ -> None) in
    let procs = proc_ls |> Map.of_alist_exn (module String) in
    let printer = Wasm_print.mk_printer type_ls in
    (*let tags = Wasm_print.prog_tags env in*)
    let compile_env = Compiler.{ type_names; procs; proc_ls = proc_ls |> List.map ~f:fst } in
    let (compile_dest, asm) = Compiler.compiler compile_env in
    let funcs = List.map proc_ls ~f:(fun (n, (d, a, b)) ->
        Printf.printf "proc %s:\n" n;
        let vars = Map.of_alist_exn (module String) a in
        let func_env = Compiler.empty_func_env () in
        func_env.need_local := Set.empty (module String);
        let (insts, e) = compile_dest ~cmd:b ~dest:d ~vars func_env in
        List.iter insts ~f:Compiler.print_macro_inst;
        Compiler.print_func_env e;
        let st =
            let locals = List.map a ~f:(fun (v, _t) -> Compiler.Addr v) in
            let stack = [] in
            let addrs = Map.of_alist_exn (module String) @@ List.map a ~f:(fun (v, _t) -> (v, [])) in
            let free_locals = [] in
            Compiler.{ stack ; locals; addrs; free_locals }
        in
        let wasm = (asm insts st e) @ [W.Call (Compiler.to_wasm_imm 0)] in
        let locals =
            let nlocals = (Set.length !(func_env.need_local)) - (List.length a) in
            List.(range 0 nlocals |> map ~f:(fun _ -> W.{ ltype = WT.NumT I32T } |> Compiler.to_region))
        in
        (* TODO: generate types *)
        let ftype = match List.length a with
        | 0 -> Compiler.type_idxs.unit_to_i32
        | 1 -> Compiler.type_idxs.i32_to_i32
        | 2 -> Compiler.type_idxs.pair_to_i32
        in
        W.{ ftype = Compiler.to_wasm_imm ftype; locals; body = wasm |> List.map ~f:Compiler.to_region }
        (*Printf.printf "WASM:\n";*)
        (*List.iter wasm ~f:(fun i -> Wasm.Print.instr Out_channel.stdout 120 @@ Compiler.to_region i);*)
        (*Printf.printf "\n";*)
        (*()*)
    )
    in
    let wasm_mod = mk_mod (printer.init_fn :: funcs) [printer.data_segment] in
    Wasm.Print.module_ (Stdio.Out_channel.stdout) 120 (Compiler.to_region wasm_mod);
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
          print_endline ((List.hd_exn (parse_cmd_line_args ())) ^ ": exception: " ^ Exn.to_string exn)
