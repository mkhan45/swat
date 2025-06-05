(** Top Level Environment *)
open Core

module U = Unix
module A = Ast
module W = Wasm.Ast
module WT = Wasm.Types

module T = Typegen

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

let mod_imports = [
    W.{ 
        module_name = Wasm.Utf8.decode "sax";
        item_name = Wasm.Utf8.decode "mem";
        idesc = W.MemoryImport WT.(MemoryT (I32AT, { min = Int64.of_int_exn 1; max = None })) |> Compiler.to_region 
    };
    W.{
        module_name = Wasm.Utf8.decode "sax";
        item_name = Wasm.Utf8.decode "alloc";
        idesc = W.FuncImport (Compiler.to_wasm_imm T.pair_to_i32) |> Compiler.to_region (* alloc *)
    };
    W.{
        module_name = Wasm.Utf8.decode "sax";
        item_name = Wasm.Utf8.decode "free";
        idesc = W.FuncImport (Compiler.to_wasm_imm T.i32_to_unit) |> Compiler.to_region (* free *)
    };
    W.{
        module_name = Wasm.Utf8.decode "sax";
        item_name = Wasm.Utf8.decode "print_val";
        idesc = W.FuncImport (Compiler.to_wasm_imm T.pair_to_unit) |> Compiler.to_region (* print *)
    }
]

let mk_mod funcs datas main_idx = W.{
    W.empty_module with 
    types = T.gen_rectypes () |> List.map ~f:Compiler.to_region;
    funcs = funcs |> List.map ~f:Compiler.to_region;
    imports = mod_imports |> List.map ~f:Compiler.to_region;
    exports = [
        { name = Wasm.Utf8.decode "serialize_types"; edesc = FuncExport (Compiler.to_wasm_imm (Compiler.fn_idxs.print_val + 1)) |> Compiler.to_region} |> Compiler.to_region;
        { name = Wasm.Utf8.decode "main"; edesc = FuncExport (Compiler.to_wasm_imm (main_idx + Compiler.fn_idxs.print_val + 2)) |> Compiler.to_region } |> Compiler.to_region;
    ];
    datas;
}

let int_tp = ("int", A.TpName "int")

let main () =
  try
    let open Core in
    let args = parse_cmd_line_args () in
    let env = load [] args in
(*  let () = print_string (A.Print.pp_env env) in *)
    let type_ls = int_tp :: (env |> List.filter_map ~f:(function A.TypeDefn (n, d) -> Some (n, d) | _ -> None)) in
    let type_names = type_ls |> Map.of_alist_exn (module String) in
    let proc_ls = env |> List.filter_map ~f:(function A.ProcDefn (n, d, a, b) -> Some (n, (d, a, b)) | _ -> None) in
    let procs = proc_ls |> Map.of_alist_exn (module String) in
    let (main_idx, _) = List.findi_exn proc_ls ~f:(fun i (n, _) -> String.equal n "main") in
    let printer = Wasm_print.mk_printer type_ls in
    (*let tags = Wasm_print.prog_tags env in*)
    let compile_env = Compiler.{ type_names; type_ls = type_ls |> List.map ~f:fst ; procs; proc_ls = proc_ls |> List.map ~f:fst; clo_funcs = ref [] } in
    let (compile_dest, asm) = Compiler.compiler compile_env in
    let funcs = List.map proc_ls ~f:(fun (n, (d, a, b)) ->
        Printf.printf "proc %s:\n" n;
        let vars = Map.of_alist_exn (module String) a in
        let (ret_dest, ret_tp) = d in
        let func_env = Compiler.empty_func_env n ret_dest in
        let (insts, e) = compile_dest ~cmd:b ~dest:d ~vars func_env in
        List.iter insts ~f:Compiler.print_macro_inst;
        Compiler.print_func_env e;
        let st =
            let locals = List.map a ~f:(fun (v, _t) -> Compiler.Addr v) in
            let stack = [] in
            let addrs = Map.of_alist_exn (module String) @@ List.map a ~f:(fun (v, _t) -> (v, [])) in
            Compiler.{ stack ; locals; addrs }
        in
        let wasm = asm insts st e in

        let wasm = match n with
        | "main" -> 
                let open Compiler in
                let (_, A.TpName dt) = d in
                let (t_idx, _) = List.findi_exn type_ls ~f:(fun i (n, _) -> String.equal dt n) in
                wasm @ 
                    [W.Const (to_wasm_int t_idx |> to_region); W.Call (fn_idxs.print_val |> to_wasm_imm); 
                    (* run alloc and free so wasm-opt doesnt remove them via dce :/ *)
                    W.Const (to_wasm_int 0 |> to_region); W.Const (to_wasm_int 0 |> to_region); W.Call (fn_idxs.alloc |> to_wasm_imm);
                    W.Call (fn_idxs.free |> to_wasm_imm);
                    W.Const (to_wasm_int 0 |> to_region)]
        | _ -> wasm
        in

        List.iter !(compile_env.clo_funcs) ~f:(fun (dest, inp, caps, wf, body) ->
            let clo_t = T.{ capture_tps = List.map caps ~f:(Fn.compose st_of_sax_tp snd);
                            inp = st_of_sax_tp @@ snd inp;
                            ret = st_of_sax_tp @@ snd dest }
            in
            let _clo_tp_idx = T.typ_idx (T.CloType clo_t) in
            ());

        let locals =
            let nlocals = !(func_env.nlocals) in
            List.(range 0 nlocals |> map ~f:(fun i ->
                let local_st = match List.Assoc.find !(func_env.local_tps) ~equal:Int.equal i with
                    | Some st -> st
                    | None -> T.I32
                in
                match local_st with
                | T.Clo ct ->
                    let idx = T.typ_idx (CloType ct) in
                    W.{ ltype = WT.RefT (NoNull, VarHT (StatX (Int32.of_int_exn idx))) } |> Compiler.to_region
                | T.I32 ->
                    W.{ ltype = WT.NumT I32T } |> Compiler.to_region))
        in

        (* TODO: generate types *)
        let ftype =
            let arg_tps = List.map a ~f:(Fn.compose T.st_of_sax_tp snd) in
            let rtp = T.st_of_sax_tp ret_tp in
            let ft = T.FuncType { inps = arg_tps; outs = [rtp] } in
            T.typ_idx ft
        in
        W.{ ftype = Compiler.to_wasm_imm ftype; locals; body = wasm |> List.map ~f:Compiler.to_region }
        (*Printf.printf "WASM:\n";*)
        (*List.iter wasm ~f:(fun i -> Wasm.Print.instr Out_channel.stdout 120 @@ Compiler.to_region i);*)
        (*Printf.printf "\n";*)
        (*()*)
    )
    in
    let clos_ls = !(compile_env.clo_funcs) in
    let clos = List.mapi clos_ls ~f:(fun idx (dest, inp, caps, wf, body) -> 
        let (dest, dest_tp) = dest in
        let var_env = caps @ [inp] in
        let st =
            let locals = Compiler.GcRef "_clo_" :: List.map var_env ~f:(fun (v, _) -> Compiler.Addr v) in
            let stack = [] in
            let addrs = Map.of_alist_exn (module String) @@ List.map var_env ~f:(fun (v, _) -> (v, [])) in
            Compiler.{ stack; locals; addrs }
        in
        let clo_t = T.{ capture_tps = List.map caps ~f:(Fn.compose st_of_sax_tp snd);
                        inp = st_of_sax_tp @@ snd inp;
                        ret = st_of_sax_tp dest_tp }
        in
        let clo_tp_idx = T.typ_idx (T.CloType clo_t) in
        let caller_tp_idx = T.typ_idx (T.CloType (T.norm_clo clo_t)) + 1 in
        let get_captures = List.concat_mapi caps ~f:(fun i (_c, _ctp) ->
            Compiler.(
                [W.LocalGet (to_wasm_imm 0); 
                W.RefCast WT.(NoNull, VarHT (StatX (Int32.of_int_exn clo_tp_idx)));
                W.StructGet (to_wasm_imm clo_tp_idx, to_wasm_imm (i+1), None);
                W.LocalSet (to_wasm_imm (i+2))]
            )
        )
        in
        let wasm = get_captures @ asm body st wf in
        let nlocals = !(wf.nlocals) + List.length (caps) + 1 in
        let locals = List.(range 0 nlocals |> map ~f:(fun _ -> W.{ ltype = WT.NumT I32T } |> Compiler.to_region)) in
        W.{ ftype = Compiler.(to_wasm_imm caller_tp_idx); locals; body = wasm |> List.map ~f:Compiler.to_region }
    )
    in
    let wasm_mod = mk_mod (printer.init_fn :: funcs @ clos) [printer.data_segment] main_idx in
    Wasm.Print.module_ (Stdio.Out_channel.create ((List.hd_exn args) ^ ".wat")) 120 (Compiler.to_region wasm_mod);
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
