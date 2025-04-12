open Core

module S = Ast
module W = Wasm.Ast
module WV = Wasm.Values

let to_region t = Wasm.Source.at Wasm.Source.no_region t

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

type wasm_func_env = { addrs : addr_map; stacklen : int }
let empty_func_env = { addrs = Map.empty (module String); stacklen = 0 }
let print_func_env env =
    let addr_ls = Map.to_alist env.addrs in
    let addr_s = String.concat ~sep:", " (List.map addr_ls ~f:(fun (s, a) -> s ^ ":" ^ (addr_to_string a))) in
    Printf.printf "addrs: { %s }\nstacklen: %d" addr_s env.stacklen

type winsts = W.instr' list

let rec sizeof (t : S.tp) : int = match t with
| One -> 0
| Times (l, r) -> sizeof l + sizeof r
| Plus ls -> 1 + (List.fold ls ~init:0 ~f:(fun max_size (_, t) -> Int.max max_size (sizeof t)))
| TpName _ -> 1

let compiler (env : compile_env) =
    let wasm_get (addr : string) (fn_env : wasm_func_env) : winsts =
        let a = Map.find_exn fn_env.addrs addr in
        match a with
        | Stack i when (i + 1) = fn_env.stacklen -> []
        | _ -> raise Todo
    in

    let rec fix (t : S.tp) : S.tp = match t with
    | S.TpName n -> (match Map.find_exn env.type_names n with
                   | S.TpName n' when String.equal n n' -> raise BadType
                   | S.TpName n' -> fix (S.TpName n')
                   | t' -> t')
    | _ -> t
    in

    let rec compile_dest ~(cmd : S.cmd) ~(dest : (string * S.tp)) ~(vars : var_env) (wasm_func : wasm_func_env) : winsts * wasm_func_env = 
        let (dest_var, dest_tp) = dest in
        let dest_tp' = fix dest_tp in
        match dest_tp' with
        | One -> (match cmd with
                  | S.Write (v, S.UnitPat) when String.equal dest_var v -> ([], wasm_func)
                  | S.Cut (v, t, l, r) ->
                          let (is, e) = compile_dest ~cmd:l ~dest:(v, t) ~vars wasm_func in
                          let new_vars = Map.set vars ~key:v ~data:t in
                          let (i, e') = compile_dest ~cmd:r ~dest ~vars:new_vars e in
                          (is @ i, e')
                  | _ -> raise TypeError)
        | Times (lt, rt) -> (match cmd with
                             | S.Write (v, S.PairPat (l, r)) when String.equal dest_var v ->
                                     (*let get_l = wasm_get l in*)
                                     (*let get_r = wasm_get r in*)
                                     let get_l = [] in
                                     let get_r = [] in
                                     let addrs = Map.set wasm_func.addrs ~key:v ~data:(Stack wasm_func.stacklen) in
                                     let stacklen = wasm_func.stacklen + (sizeof dest_tp') in
                                     ((get_l @ get_r), { wasm_func with stacklen; addrs })
                             | S.Cut (v, t, l, r) ->
                                     let (is, e) = compile_dest ~cmd:l ~dest:(v, t) ~vars wasm_func in
                                     let new_vars = Map.set vars ~key:v ~data:t in
                                     let (i, e') = compile_dest ~cmd:r ~dest ~vars:new_vars e in
                                     (is @ i, e')
                             | _ -> raise TypeError)
        | Plus ls -> (match cmd with
                      | S.Write (v, S.InjPat (l, v')) when String.equal dest_var v && List.exists ls ~f:(fun (l', _) -> String.equal l l') ->
                            (*let get_v = wasm_get v' in*)
                            let get_v = [] in
                            let addrs = Map.set wasm_func.addrs ~key:v ~data:(Stack wasm_func.stacklen) in
                            let tag_idx = List.findi ls ~f:(fun i (l', _) -> String.equal l l') |> Option.value_exn |> fst in
                            let push_idx = W.Const (to_region @@ to_wasm_int tag_idx) in
                            let stacklen = wasm_func.stacklen + (sizeof dest_tp') in
                            (push_idx :: get_v, { wasm_func with stacklen; addrs })
                      | S.Cut (v, t, l, r) ->
                              let (is, e) = compile_dest ~cmd:l ~dest:(v, t) ~vars wasm_func in
                              let new_vars = Map.set vars ~key:v ~data:t in
                              let (i, e') = compile_dest ~cmd:r ~dest ~vars:new_vars e in
                              (is @ i, e')
                      | _ -> raise TypeError)
        | _ -> raise Todo
    in

    compile_dest
    (*| Times (lt, rt) ->*)
