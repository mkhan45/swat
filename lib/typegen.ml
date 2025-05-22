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

let type_decls : (type_def list) ref = ref []

let typ_idx (t : type_def) : int =
    let search = List.findi (!type_decls) ~f:(fun _ t' -> equal_type_def t t') in
    match search with
    | Some (i, _) -> i
    | None -> 
            type_decls := (!type_decls) @ [t];
            List.length (!type_decls) - 1

let gen_rectype : WT.rec_type =
    let rec loop (remaining : type_def list) : WT.str_type list = match remaining with
    | FuncType f :: xs -> raise Todo
    | CloType c :: xs -> raise Todo
    in
    let def_types = loop (!type_decls) in
    let sub_ts = List.map def_types ~f:(fun dt -> WT.(SubT (Final, [], dt))) in
    WT.RecT sub_ts
