open Core

module S = Ast
module WT = Wasm.Types
module WV = Wasm.Value

exception Todo

(* types of fn args or returns *)
type stack_type = I32
                | Clo of clo_type
                [@@deriving eq]

and clo_type = { capture_tps : stack_type list; inp: stack_type; ret: stack_type } [@@deriving eq]
and func_type = { inps : stack_type list; outs : stack_type list } [@@deriving eq]

and type_def = FuncType of func_type
             | CloType of clo_type
             [@@deriving eq]

let rec st_of_sax_tp (sax_tp : S.tp) : stack_type = match sax_tp with
| Arrow (i, o) -> Clo { capture_tps = []; inp = st_of_sax_tp i; ret = st_of_sax_tp o }
| _ -> I32

let type_decls : (type_def list) ref = ref []

let typ_idx (t : type_def) : int =
    let search = List.findi (!type_decls) ~f:(fun _ t' -> equal_type_def t t') in
    match search with
    | Some (i, _) -> i
    | None -> 
            (match t with
            | FuncType _ ->
                type_decls := (!type_decls) @ [t];
                List.length (!type_decls) - 1
            | CloType ct ->
                let caller_t = FuncType { inps = [Clo ct; ct.inp]; outs = [ct.ret] } in
                type_decls := (!type_decls) @ [t; caller_t];
                List.length (!type_decls) - 2)

let ser_stack_type (st : stack_type) : WT.val_type = match st with
| I32 -> WT.(NumT I32T)
| Clo ct -> 
    let i = typ_idx (CloType ct) in
    WT.(RefT (NoNull, VarHT (StatX (Int32.of_int_exn i))))

let ser_func_type (ft : func_type) : WT.func_type = 
    let inp_ts = List.map ft.inps ~f:ser_stack_type in
    let out_ts = List.map ft.outs ~f:ser_stack_type in
    (FuncT (inp_ts, out_ts))

let gen_rectypes () : WT.rec_type list =
    let rec loop (idx : int) : (WT.str_type list) list=
    if idx = List.length (!type_decls) then [] else
    match List.nth_exn (!type_decls) idx with
    | FuncType f -> [WT.(DefFuncT (ser_func_type f))] :: loop (idx + 1)
    | CloType c -> 
        let self_call_ftp = { inps = [(Clo c); c.inp]; outs = [c.ret] } in
        let self_call_tp = FuncType self_call_ftp in
        let self_call_def = WT.DefFuncT (ser_func_type self_call_ftp) in
        let self_call_idx = typ_idx self_call_tp in
        let self_call_val_tp = WT.(RefT (NoNull, VarHT (StatX (Int32.of_int_exn self_call_idx)))) in
        let capture_tps = List.map c.capture_tps ~f:ser_stack_type in
        let field_ts = List.map (self_call_val_tp :: capture_tps) ~f:(fun ct -> WT.(FieldT (Cons, ValStorageT ct))) in
        let struct_t = WT.StructT (field_ts) in
        let struct_def = WT.DefStructT struct_t in
        [struct_def; self_call_def] :: loop (idx + 2)
    in
    let def_types = loop 0 in
    List.map def_types ~f:(fun dts -> WT.(RecT (List.map dts ~f:(fun dt -> SubT (Final, [], dt)))))

let pair_to_i32 = typ_idx (FuncType { inps = [I32; I32]; outs = [I32] })
let i32_to_unit = typ_idx (FuncType { inps = [I32]; outs = [] })
let pair_to_unit = typ_idx (FuncType { inps = [I32; I32]; outs = [] })
let unit_to_i32 = typ_idx (FuncType { inps = []; outs = [I32] })

let test () : unit =
    let ct = { capture_tps = [I32]; inp = I32; ret = I32 } in
    let t1 = CloType { capture_tps = [I32]; inp = I32; ret = I32 } in
    let t2 = FuncType { inps = []; outs = [Clo ct] } in
    let t3 = FuncType { inps = [I32]; outs = [Clo ct] } in
    let _ = typ_idx t1 in
    let _ = typ_idx t2 in
    let _ = typ_idx t3 in
    let rec_ts = gen_rectypes () in
    List.iter rec_ts ~f:(fun rec_t ->
        let s = WT.string_of_rec_type rec_t in
        Printf.printf "rect: %s\n" s)
