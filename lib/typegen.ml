open Core

module S = Ast
module WT = Wasm.Types
module WV = Wasm.Value

exception Todo

(* types of fn args or returns *)
type stack_type = I32
                | Clo of clo_type

type clo_type = { capture_tps : stack_type list; inp: stack_type; ret: stack_type }
type func_type = { inps : stack_type list; outs : stack_type list }

let func_types = (func_type list) ref
let clo_types = (clo_type list) ref

let func_typ_idx (ftp : func_type) : int = raise Todo
let clo_typ_idx (ftp : func_type) : int = raise Todo
