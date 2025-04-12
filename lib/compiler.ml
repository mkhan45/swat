open Core

module S = Ast
module W = Wasm.Ast
module WV = Wasm.Value

let to_region t = Wasm.Source.(@@) t Wasm.Source.no_region

let to_wasm_int i = WV.I32 (Int32.of_int_exn i)

exception Todo
exception BadType

exception TypeError

type type_name_map = (string, S.tp, String.comparator_witness) Map.t
type proc_map = (string, (S.parm * S.parm list * S.cmd), String.comparator_witness) Map.t

type compile_env = { type_names : type_name_map; procs : proc_map }

type var_env = (string, S.tp, String.comparator_witness) Map.t

type wasm_addr = Stack of int
               | Local of int
               | Heap of int
let addr_to_string = function
    | Stack i -> Printf.sprintf "S %d" i
    | Local i -> Printf.sprintf "L %d" i
    | Heap i -> Printf.sprintf "H %d" i

type addr_map = (string, wasm_addr, String.comparator_witness) Map.t

type wasm_func_env = ()
let empty_func_env = ()
let print_func_env env = ()
    (*let addr_ls = Map.to_alist env.addrs in*)
    (*let addr_s = String.concat ~sep:", " (List.map addr_ls ~f:(fun (s, a) -> s ^ ":" ^ (addr_to_string a))) in*)
    (*Printf.printf "addrs: { %s }\nstacklen: %d" addr_s env.stacklen*)

type winsts = W.instr' list

let rec sizeof (t : S.tp) : int = match t with
| One -> 0
| Times (l, r) -> sizeof l + sizeof r
| Plus ls -> 1 + (List.fold ls ~init:0 ~f:(fun max_size (_, t) -> Int.max max_size (sizeof t)))
| TpName _ -> 1

type macro_inst = GetAddr of string
                | InitAddr of string
                | Move of (string * string)
                | WASM of W.instr'

(*type asm_env = *)

let compiler (env : compile_env) =
    let rec fix (t : S.tp) : S.tp = match t with
    | S.TpName n -> (match Map.find_exn env.type_names n with
                   | S.TpName n' when String.equal n n' -> raise BadType
                   | S.TpName n' -> fix (S.TpName n')
                   | t' -> t')
    | _ -> t
    in

    let rec asm (ms : macro_inst list) (wf : wasm_func_env) : W.instr' list = match ms with
    | [] -> []
    | (WASM w) :: xs -> w :: asm xs wf
    | (GetAddr s) :: xs -> raise Todo
    | (InitAddr s) :: xs -> raise Todo
    in

    let rec compile_dest ~(cmd : S.cmd) ~(dest : (string * S.tp)) ~(vars : var_env) (wasm_func : wasm_func_env) : (macro_inst list) * wasm_func_env = 
        match cmd with
        | S.Cut (v, t, l, r) ->
              (* TODO: typecheck *)
              let (is, e) = compile_dest ~cmd:l ~dest:(v, t) ~vars wasm_func in
              let new_vars = Map.set vars ~key:v ~data:t in
              let (i, e') = compile_dest ~cmd:r ~dest ~vars:new_vars e in
              (is @ ((InitAddr v) :: i), e')
        | S.Id (l, r) ->
              (* TODO: typecheck *)
              ([Move (l, r)], wasm_func)
        | S.Call (p, d, ps) ->
              (* TODO *)
              raise Todo
        | _ ->
            let (dest_var, dest_tp) = dest in
            let dest_tp' = fix dest_tp in
            (match dest_tp' with
            | One -> (match cmd with
                      | S.Write (v, S.UnitPat) when String.equal dest_var v -> ([], wasm_func)
                      | _ -> raise TypeError)
            | Times (lt, rt) -> (match cmd with
                                 | S.Write (v, S.PairPat (l, r)) when String.equal dest_var v ->
                                         ([GetAddr l; GetAddr r], ())
                                 | _ -> raise TypeError)
            | Plus ls -> (match cmd with
                          | S.Write (v, S.InjPat (l, v')) when String.equal dest_var v && List.exists ls ~f:(fun (l', _) -> String.equal l l') ->
                                let tag_idx = List.findi ls ~f:(fun i (l', _) -> String.equal l l') |> Option.value_exn |> fst in
                                let push_idx = WASM (W.Const (to_region @@ to_wasm_int tag_idx)) in
                                ([push_idx; GetAddr v'], ())
                          | _ -> raise TypeError)
            | _ -> raise Todo)
    in

    compile_dest
    (*| Times (lt, rt) ->*)
