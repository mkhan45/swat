open Core

module S = Ast
module W = Wasm.Ast

exception Todo

type type_name_map = (string, S.tp, String.comparator_witness) Map.t
type proc_map = (string, (S.parm * S.parm list * S.cmd), String.comparator_witness) Map.t

type compile_env = { type_names : type_name_map; procs : proc_map; }

let compiler (env : compile_env) =
    let rec compile (sax : S.cmd) : W.instr list = raise Todo in
    raise Todo
