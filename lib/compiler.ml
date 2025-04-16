open Core

module S = Ast
module W = Wasm.Ast
module WV = Wasm.Value

let to_region t = Wasm.Source.(@@) t Wasm.Source.no_region

let to_wasm_int i = WV.I32 (Int32.of_int_exn i)
let to_wasm_imm i = to_region (Int32.of_int_exn i)

exception Todo
exception BadType
exception BadMatch

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


type stack_val = Addr of string
               | InjTag of string
               | InjData of string
               | PairFst of string
               | PairSnd of string
               | Unit of string

type addr_rel = Pair of (string * string)
              | PairFst of string
              | PairSnd of string
              | Inj of string
              | InjVal of string

type addr_graph = (string, addr_rel list, String.comparator_witness) Map.t

let is_addr s = function
| Addr s' when String.equal s s' -> true
| _ -> false

let add_rel (graph : addr_graph) ~(addr : string) ~(rel : addr_rel) : addr_graph = match Map.find graph addr with
| Some ls -> Map.set graph ~key:addr ~data:(rel :: ls)
| None -> Map.set graph ~key:addr ~data:[rel]

type asm_state = { stack : stack_val list; locals : stack_val list; addrs : addr_graph }

type winsts = W.instr' list

let rec sizeof (t : S.tp) : int = match t with
| One -> 0
| Times (l, r) -> sizeof l + sizeof r
| Plus ls -> 1 + (List.fold ls ~init:0 ~f:(fun max_size (_, t) -> Int.max max_size (sizeof t)))
| TpName _ -> 1

type macro_inst = GetAddr of string (* ensures ref of name is top of stack *)
                | GetAddrTag of string (* ensures tag of address is top of stack *)
                | InitAddr of string (* allocates a cell with name *)
                | AliasInj of (string * string)
                | AliasPair of (string * (string * string))
                | Move of (string * string) (* moves values between refs *)
                | Call of (string * string * string list)
                | Switch of ((int * (macro_inst list)) list) (* switches on top of stack *)
                | PushTag of (int * string)
                | PushUnit of string

let rec print_macro_inst = function
| GetAddr s -> Printf.printf "Get %s\n" s
| GetAddrTag s -> Printf.printf "GetTag %s\n" s
| PushTag (i, s) -> Printf.printf "PushTag %d %s\n" i s
| PushUnit s -> Printf.printf "PushUnit %s\n" s
| InitAddr s -> Printf.printf "Init %s\n" s
| AliasInj (l, r) -> Printf.printf "Alias %s to %s.inj\n" r l
| AliasPair (p, (l, r)) -> Printf.printf "Alias (%s, %s) to %s" l r p
| Move (l, r) -> Printf.printf "Move %s to %s\n" l r
| Call (p, d, ps) -> Printf.printf "Call %s (%s) into %s\n" p (String.concat ~sep:", " ps) d
| Switch ls -> 
        Printf.printf "Switch\n";
        List.iter ls ~f:(fun (i, is) ->
            Printf.printf "Case %d:\n" i;
            List.iter is ~f:print_macro_inst)

let compiler (env : compile_env) =
    let rec fix (t : S.tp) : S.tp = match t with
    | S.TpName n -> (match Map.find_exn env.type_names n with
                   | S.TpName n' when String.equal n n' -> raise BadType
                   | S.TpName n' -> fix (S.TpName n')
                   | t' -> t')
    | _ -> t
    in

    let rec asm (ms : macro_inst list) (st : asm_state) (wf : wasm_func_env) : W.instr' list = match ms with
    | [] -> []
    | (PushUnit s) :: xs -> (W.Const (to_wasm_int 0 |> to_region)) :: asm xs { st with stack = (Unit s) :: st.stack } wf
    | (PushTag (i, s)) :: xs -> (W.Const (to_wasm_int i |> to_region)) :: asm xs { st with stack = (InjTag s) :: st.stack } wf
    | (AliasInj (src, dst)) :: xs ->
            let src_addr_rel = Inj dst in
            let dst_addr_rel = InjVal src in
            let graph = st.addrs |> add_rel ~addr:src ~rel:src_addr_rel |> add_rel ~addr:dst ~rel:dst_addr_rel in
            asm xs { st with addrs = graph } wf
    | (GetAddr s) :: xs ->
            (match st.stack with
             | (Addr v) :: ss when String.equal v s -> asm xs st wf (* already on top of stack *)
             | _ -> (match List.findi st.locals ~f:(fun _i v -> is_addr s v) with
                     | Some (i, _) -> (W.LocalGet (to_wasm_imm i)) :: asm xs st wf
                     | None -> raise Todo (* probably err *)))
    | (InitAddr s) :: xs ->
            (* don't alloc until write *)
            let i = W.Const (to_wasm_int 42 |> to_region) in (* TODO: call alloc *)
            (match st.stack with
             | (Unit _) :: rest -> i :: asm xs { st with stack = (Addr s) :: rest } wf
             | _ :: _ :: rest -> i :: asm xs { st with stack = (Addr s) :: rest } wf)
    | (Call (p, d, _ps)) :: xs -> (* parms should already be pushed *)
            let proc_ls = Map.to_alist env.procs in
            let (idx, proc) = List.findi_exn proc_ls ~f:(fun _i (p', _) -> String.equal p p') in
            let (_, ((_d, proc_dest_tp), proc_parms, _)) = proc in
            let stack =
                let popn = List.length proc_parms in
                let rest = List.drop st.stack popn in
                (match (fix proc_dest_tp) with
                 | One -> (Unit d) :: rest
                 | Times (_, _) -> (PairSnd d) :: (PairFst d) :: rest
                 | Plus _ -> (InjTag d) :: (InjData d) :: rest)
            in
            (W.Call (to_wasm_imm idx)) :: asm xs { st with stack } wf
     | (Switch ls) :: xs -> raise Todo
    in

    let rec compile_dest ~(cmd : S.cmd) ~(dest : (string * S.tp)) ~(vars : var_env) (wasm_func : wasm_func_env) : (macro_inst list) * wasm_func_env = 
        match cmd with
        | S.Cut (v, t, l, r) ->
              (* TODO: typecheck *)
              let (is, e) = compile_dest ~cmd:l ~dest:(v, t) ~vars wasm_func in
              let new_vars = Map.set vars ~key:v ~data:t in
              let (i, e') = compile_dest ~cmd:r ~dest ~vars:new_vars e in
              ((is @ (InitAddr v :: i)), e')
        | S.Id (l, r) ->
              (* TODO: typecheck 
                    - does l have to be dest? *)
              ([Move (l, r)], wasm_func)
        | S.Call (p, d, ps) ->
              (* TODO:
                    - does d have to be dest? I think so *)
              let gets = List.map ps ~f:(fun v -> GetAddr v) in
              (gets @ [Call (p, d, ps)], wasm_func)
        | S.Read (v, bs) ->
              let subj_tp = Map.find_exn vars v |> fix in
              (match subj_tp with
              | One -> 
                      (* TODO: typecheck *)
                      ([], wasm_func)
              | Times (lt, rt) -> 
                      (match bs with
                       | [PairPat (lv, rv), b] -> 
                               let inner_env = vars |> Map.set ~key:lv ~data:lt |> Map.set ~key:rv ~data:rt in
                               let (is, _e) = compile_dest ~cmd:b ~dest:dest ~vars:inner_env wasm_func in
                               ((AliasPair (v, (lv, rv)) :: is), wasm_func)
                       | _ -> raise BadMatch)
              | Plus ls -> 
                      let bs = List.map bs ~f:(function (InjPat (l, v), b) -> (l, (v, b)) | _ -> raise BadMatch) in
                      let cases = List.map ls ~f:(fun (l, t) -> (l, List.Assoc.find_exn bs l ~equal:String.equal)) in
                      let case_instrs = List.mapi cases ~f:(fun i (l, (v', b)) -> 
                          let inner_env = Map.set vars ~key:v' ~data:(List.Assoc.find_exn ls l ~equal:String.equal) in
                          let (is, _e) = compile_dest ~cmd:b ~dest:dest ~vars:inner_env wasm_func in
                          (i, (AliasInj (v, v')) :: is))
                      in
                      ([GetAddrTag v; Switch case_instrs], wasm_func))
        | _ ->
            let (dest_var, dest_tp) = dest in
            let dest_tp' = fix dest_tp in
            (match dest_tp' with
            | One -> (match cmd with
                      | S.Write (v, S.UnitPat) when String.equal dest_var v -> ([PushUnit v], wasm_func)
                      | _ -> raise TypeError)
            | Times (lt, rt) -> (match cmd with
                                 | S.Write (v, S.PairPat (l, r)) when String.equal dest_var v ->
                                         ([GetAddr l; GetAddr r], ())
                                 | _ -> raise TypeError)
            | Plus ls -> (match cmd with
                          | S.Write (v, S.InjPat (l, v')) when String.equal dest_var v && List.exists ls ~f:(fun (l', _) -> String.equal l l') ->
                                let tag_idx = List.findi ls ~f:(fun i (l', _) -> String.equal l l') |> Option.value_exn |> fst in
                                let push_idx = PushTag (tag_idx, v) in
                                ([GetAddr v'; push_idx], ())
                          | _ -> raise TypeError)
            | _ -> raise Todo)
    in

    (compile_dest, asm)
    (*| Times (lt, rt) ->*)
