open Core

module S = Ast
module W = Wasm.Ast
module WV = Wasm.Value
module WT = Wasm.Types
module WS = Wasm.V128

module T = Typegen

let to_region t = Wasm.Source.(@@) t Wasm.Source.no_region

let to_wasm_int i = WV.I32 (Int32.of_int_exn i)
let to_wasm_imm i = to_region (Int32.of_int_exn i)

let wasm_load_fst = W.Load ((to_wasm_imm 0), W.{ ty = WT.I32T; align = 2; offset = Int64.of_int_exn 0; pack = None })
let wasm_load_snd = W.Load ((to_wasm_imm 0), W.{ ty = WT.I32T; align = 2; offset = Int64.of_int_exn 4; pack = None })
(*let wasm_load_whole = [*)
(*    W.VecLoad ((to_wasm_imm 0), W.{ ty = WT.V128T; align = 2; offset = Int64.of_int_exn 0; pack = None });*)
(*    W.VecExtract (V128 (WS.I32x4 (Extract (0, ()))))*)
(*    W.VecExtract (V128 (WS.I32x4 (Extract (1, ()))))*)
(*]*)

exception Todo
exception BadType
exception BadMatch

exception TypeError

type type_name_map = (string, S.tp, String.comparator_witness) Map.t
type proc_map = (string, (S.parm * S.parm list * S.cmd), String.comparator_witness) Map.t

type macro_inst = GetAddr of string (* ensures ref of name is top of stack *)
                | GetAddrTag of string (* ensures tag of address is top of stack *)
                | GetFunc of int
                | GetRef of string
                | InitAddr of (string * T.stack_type) (* allocates a cell with name *)
                | AliasInj of (string * string)
                | AliasPair of (string * (string * string))
                | DerefPair of string
                | Move of (string * string) (* moves values between refs *)
                | Call of (string * string * string list)
                | InvokeClo of (string * string * int) (* arg, dest, clo tp *)
                | Switch of (string * bool * (int * (macro_inst list)) list) (* switches on top of stack *)
                | PushTag of (int * string)
                | PushUnit of string
                | GetUnit of string
                | PushInt of (int * string)
                | InitClo of (int * int * int * string) (* tp idx, func idx, ncaps, addr *)

type string_set = (string, String.comparator_witness) Set.t

type wasm_func_env = { 
    read_in_func : string_set ref;
    cut_in_func : string_set ref;
    nlocals : int ref;
    local_tps : ((int * T.stack_type) list) ref;
    name : string;
    ret_dest : string 
}
type clo_func = (S.parm * S.parm * (S.parm list) * wasm_func_env * macro_inst list) (* dest, inp, capture args, wf, body *)
type compile_env = { type_names : type_name_map; type_ls : string list; procs : proc_map; proc_ls : string list; clo_funcs : (clo_func list) ref }

type type_idx_map = {
    unit_fn : int;
    i32_to_unit : int; (* print *)
    i32_to_i32  : int; (* alloc, sax functions currently *)
    i32_to_pair : int; (* sax functions eventually *)
    unit_to_i32 : int; (* serialize types *)
    pair_to_unit : int;
    pair_to_i32 : int;
    ref_i32_to_i32 : int; (* lifted closure *)
    unit_to_ref : int; (* func returning clo *)
    i32_to_ref : int; (* func accepting one arg and returning clo *)
}

type fn_idx_map = {
    alloc : int;
    free : int;
    print_val : int;
}

let fn_idxs : fn_idx_map = {
    alloc = 1; free = 2; print_val = 0;
}

type var_env = (string, S.tp, String.comparator_witness) Map.t

let empty_func_env name ret_dest = { read_in_func = ref @@ Set.empty (module String); cut_in_func = ref @@ Set.empty (module String); nlocals = ref 0; local_tps = ref []; name; ret_dest }
let print_func_env env = ()
    (*let addr_ls = Map.to_alist env.addrs in*)
    (*let addr_s = String.concat ~sep:", " (List.map addr_ls ~f:(fun (s, a) -> s ^ ":" ^ (addr_to_string a))) in*)
    (*Printf.printf "addrs: { %s }\nstacklen: %d" addr_s env.stacklen*)

type stack_val = Addr of string
               | GcRef of string
               | InjTag of string
               | InjData of string
               | PairFst of string
               | PairSnd of string
               | Unit of string
               | Int of string

let print_stack_val : stack_val -> string = function
| Addr s -> "adr " ^ s
| GcRef s -> "gcr " ^ s
| InjTag s -> "tag " ^ s
| InjData s -> "inj " ^ s
| PairFst s -> "fst " ^ s
| PairSnd s -> "snd " ^ s
| Unit s -> "unt " ^ s
| Int s -> "int " ^ s

let print_stack (s : stack_val list) : string = String.concat ~sep:", " @@ List.map s ~f:print_stack_val

let stack_pair (a : string) : stack_val list = [PairFst a; PairSnd a]
let stack_inj (a : string) : stack_val list = [InjTag a; InjData a]

type addr_rel = Pair of (string * string)
              | PairFst of string
              | PairSnd of string
              | Inj of string
              | InjVal of string

type addr_graph = (string, addr_rel list, String.comparator_witness) Map.t

type asm_state = { stack : stack_val list; locals : stack_val list; addrs : addr_graph }

let is_addr s = function
| Addr s' when String.equal s s' -> true
| _ -> false

let is_tag s = function
| InjTag s' when String.equal s s' -> true
| _ -> false

let is_inj s st = function
| InjData s' when String.equal s s' -> true
| Addr s'' -> 
        Option.value_map ~default:false (Map.find st.addrs s'') ~f:(fun rels ->
                    List.exists rels ~f:(function
                    | InjVal s' when String.equal s s' -> true
                    | _ -> false))
| _ -> false

let is_fst (s : string) (st : asm_state) : stack_val -> bool = function
| PairFst s' when String.equal s s' -> true
| Addr s'' -> 
        Option.value_map ~default:false (Map.find st.addrs s'') ~f:(fun rels ->
                    List.exists rels ~f:(function
                    | PairFst s' when String.equal s s' -> true
                    | _ -> false))
| _ -> false

let is_snd (s : string) (st : asm_state) : stack_val -> bool = function
| PairSnd s' when String.equal s s' -> true
| Addr s'' -> Option.value_map ~default:false (Map.find st.addrs s'') ~f:(fun rels ->
                    List.exists rels ~f:(function
                    | PairSnd s' when String.equal s s' -> true
                    | _ -> false))
| _ -> false

let add_rel (graph : addr_graph) ~(addr : string) ~(rel : addr_rel) : addr_graph = match Map.find graph addr with
| Some ls -> Map.set graph ~key:addr ~data:(rel :: ls)
| None -> Map.set graph ~key:addr ~data:[rel]

type winsts = W.instr' list

let rec sizeof (t : S.tp) : int = match t with
| One -> 0
| Times (l, r) -> sizeof l + sizeof r
| Plus ls -> 1 + (List.fold ls ~init:0 ~f:(fun max_size (_, t) -> Int.max max_size (sizeof t)))
| TpName _ -> 1

let rec print_macro_inst = function
| GetAddr s -> Printf.printf "Get %s\n" s
| GetAddrTag s -> Printf.printf "GetTag %s\n" s
| GetFunc s -> Printf.printf "GetFunc %d\n" s
| GetRef s -> Printf.printf "GetRef %s\n" s
| PushTag (i, s) -> Printf.printf "PushTag %d %s\n" i s
| PushInt (i, s) -> Printf.printf "PushInt %d %s\n" i s
| PushUnit s -> Printf.printf "PushUnit %s\n" s
| GetUnit s -> Printf.printf "GetUnit %s\n" s
| InitAddr (s, _) -> Printf.printf "Init %s\n" s
| InitClo (tp_idx, fn_idx, nargs, addr) -> Printf.printf "InitClo tp:%d fn:%d %s\n" tp_idx fn_idx addr
| InvokeClo (arg, dest, _) -> Printf.printf "InvokeClo(%s), %s\n" arg dest
| AliasInj (l, r) -> Printf.printf "Alias %s to %s.inj\n" r l
| AliasPair (p, (l, r)) -> Printf.printf "Alias (%s, %s) to %s\n" l r p
| DerefPair (p) -> Printf.printf "DerefPair %s\n" p
| Move (l, r) -> Printf.printf "Move %s to %s\n" r l
| Call (p, d, ps) -> Printf.printf "Call %s (%s) into %s\n" p (String.concat ~sep:", " ps) d
| Switch (s, need_inj, ls) -> 
        Printf.printf "Switch (%s) [need_inj: %b]\n" s need_inj;
        List.iter ls ~f:(fun (i, is) ->
            Printf.printf "Case %d:\n" i;
            List.iter is ~f:print_macro_inst)

let rec fix (env : compile_env) (t : S.tp) : S.tp = match t with
| S.TpName "int" -> t
| S.TpName n -> (match Map.find_exn env.type_names n with
               | S.TpName n' when String.equal n n' -> raise BadType
               | S.TpName n' -> fix env (S.TpName n')
               | t' -> t')
| _ -> t

let compiler (env : compile_env) =
    let fix = fix env in

    let rec asm (ms : macro_inst list) (st : asm_state) (wf : wasm_func_env) : W.instr' list = 
        (* TODO: proper search *)
        let rec get_addr (s : string) : W.instr' list = match st.stack with
        | (Addr v) :: _ when String.equal v s -> [] (* already on top of stack *)
        | (Unit v) :: _ when String.equal v s -> [] (* already on top of stack *)
        | _ -> (match List.findi st.locals ~f:(fun _i v -> is_addr s v) with
                | Some (i, _) -> [W.LocalGet (to_wasm_imm i)]
                | None ->
                    Printf.printf "looking for %s, locals: %s\n" s (print_stack st.locals);
                    let res = Option.bind (Map.find st.addrs s) ~f:(fun rels ->
                        List.find_map rels ~f:(function
                        | InjVal s -> Some (get_inj s)
                        | PairFst s -> Some (get_fst s)
                        | PairSnd s -> Some (get_snd s)
                        | _ -> None))
                    in
                    match res with
                    | Some is -> is
                    | _ -> raise Todo)

        (* TODO: any case that loads should load the whole thing instead of just components,
                 and then free the addr. If we're loading the fst, it should load in backwards order *)
        and get_tag (s : string) : W.instr' list = match st.stack with
        | (InjTag v) :: _ when String.equal v s -> [] (* already on top of stack *)
        | _ -> (match List.findi st.locals ~f:(fun _i v -> is_tag s v) with
                | Some (i, _) -> [W.LocalGet (to_wasm_imm i)]
                | None -> (get_addr s) @ [wasm_load_snd] )

        and get_inj (s : string) : W.instr' list = match st.stack with
        | (InjData v) :: _ when String.equal v s -> [] (* already on top of stack *)
        | _ -> (match List.findi st.locals ~f:(fun _i v -> is_inj s st v) with
                | Some (i, _) -> [W.LocalGet (to_wasm_imm i)]
                | None -> (get_addr s) @ [wasm_load_fst] )

        and get_fst (s : string) : W.instr' list = match st.stack with
        | (PairFst v) :: _ when String.equal v s -> [] (* already on top of stack *)
        | _ -> (match List.findi st.locals ~f:(fun _i v -> is_fst s st v) with
                | Some (i, _) -> [W.LocalGet (to_wasm_imm i)]
                | None -> (get_addr s) @ [wasm_load_fst] )

        and get_snd (s : string) : W.instr' list = match st.stack with
        | (PairSnd v) :: _ when String.equal v s -> [] (* already on top of stack *)
        | _ -> (match List.findi st.locals ~f:(fun _i v -> is_snd s st v) with
                | Some (i, _) -> [W.LocalGet (to_wasm_imm i)]
                | None -> (get_addr s) @ [wasm_load_snd] )

        and get_unit (s : string) : W.instr' list = match st.stack with
        | (Unit v) :: _ when String.equal v s -> [] (* already on top of stack *)
        | _ -> [W.Const (to_wasm_int 0 |> to_region)]

        and put_local (st : asm_state) : W.instr' list * asm_state = match st.stack with
        | sv :: ss ->
                let local_idx = List.length st.locals in
                let new_locals = st.locals @ [sv] in
                (* TODO: should adjust wasm_func to have local_types *)
                ([W.LocalSet (to_wasm_imm local_idx)], { st with stack = ss; locals = new_locals })
        in

        match ms with
        | [] -> (match st.stack with
                 | (Addr _ | Int _ | Unit _ | GcRef _) :: [] -> if not (String.equal "main" wf.name) then [W.Return] else []
                 | _ :: _ :: [] -> if not (String.equal "main" wf.name) then [W.ReturnCall (to_wasm_imm fn_idxs.alloc)] else [W.Call (to_wasm_imm fn_idxs.alloc)]
                 | (InjTag s) :: [] ->
                         (* special case, it's a bool, put a 0 in front of it and alloc *)
                        let (i1, st') = put_local st in
                        wf.nlocals := Int.max !(wf.nlocals) (List.length st'.locals);
                        let (local_idx, _) = List.findi_exn st'.locals ~f:(fun _i v -> is_tag s v) in
                        i1 @ [W.Const (to_wasm_int 0 |> to_region); W.LocalGet (to_wasm_imm local_idx); W.ReturnCall (to_wasm_imm fn_idxs.alloc)]
                 | _ ->
                         Printf.printf "err: [%s]\n" @@ print_stack st.stack;
                         raise Todo
                         )
        | (PushUnit s) :: xs -> (W.Const (to_wasm_int 0 |> to_region)) :: asm xs { st with stack = (Unit s) :: st.stack } wf
        | (PushTag (i, s)) :: xs -> (W.Const (to_wasm_int i |> to_region)) :: asm xs { st with stack = (InjTag s) :: st.stack } wf
        | (PushInt (i, s)) :: xs -> (W.Const (to_wasm_int i |> to_region)) :: asm xs { st with stack = (Addr s) :: st.stack } wf
        | (Move (src, dst)) :: xs -> 
                (match st.stack with
                 | Addr s :: rest when String.equal src s -> asm xs { st with stack = (Addr dst) :: rest } wf)
        | (AliasInj (src, dst)) :: xs ->
                let src_addr_rel = Inj dst in
                let dst_addr_rel = InjVal src in
                let graph = st.addrs |> add_rel ~addr:src ~rel:src_addr_rel |> add_rel ~addr:dst ~rel:dst_addr_rel in
                asm xs { st with addrs = graph } wf
        | (AliasPair (src, (fst, snd))) :: xs ->
                let src_addr_rel = Pair (fst, snd) in
                let fst_addr_rel = PairFst src in
                let snd_addr_rel = PairSnd src in
                let graph = st.addrs 
                            |> add_rel ~addr:src ~rel:src_addr_rel 
                            |> add_rel ~addr:fst ~rel:fst_addr_rel 
                            |> add_rel ~addr:snd ~rel:snd_addr_rel
                in
                asm xs { st with addrs = graph } wf
                (*let need_locals = (Set.mem !(wf.need_local) fst) || (Set.mem !(wf.need_local) snd) in*)
                (*begin if need_locals then*)
                (*    (* maybe non-tail recurse, add GetAddr etc to front*)
                (*       should also probably have GetFst and GetSnd *)*)
                (*    let (i1, st') = put_local st in*)
                (*    let (i2, st'') = put_local st' in*)
                (*    (i1 @ i2) @ asm xs st' wf*)
                (*else*)
                (*    asm xs { st with addrs = graph } wf*)
                (*end*)
        | (DerefPair s) :: xs -> 
                if not ((Set.mem !(wf.read_in_func) s) && (Set.mem !(wf.cut_in_func) s)) then
                    let get_fst = get_fst s in
                    let (put_fst, st') = put_local { st with stack = (PairFst s) :: st.stack } in
                    let get_snd = get_snd s in
                    let (put_snd, st'') = put_local { st' with stack = (PairSnd s) :: st'.stack } in
                    let get_addr = get_addr s in
                    let free = [W.Call (to_wasm_imm fn_idxs.free)] in
                    wf.nlocals := Int.max !(wf.nlocals) (List.length st''.locals);
                    (get_fst @ put_fst @ get_snd @ put_snd @ get_addr @ free) @ asm xs st'' wf
                else
                    (* already in locals *)
                    asm xs st wf
        | (GetUnit s) :: xs ->
                (match get_unit s with
                                | [] -> asm xs st wf (* already on stack *)
                                | is -> is @ asm xs { st with stack = (Unit s) :: st.stack } wf)
        | (GetAddr s) :: xs -> 
                (match get_addr s with
                                | [] -> asm xs st wf (* already on stack *)
                                | is -> is @ asm xs { st with stack = (Addr s) :: st.stack } wf)
        | (GetAddrTag s) :: xs -> (match get_tag s with
                                   | [] -> asm xs st wf (* already on stack *)
                                   | is -> is @ asm xs { st with stack = (InjTag s) :: st.stack } wf)
        | (GetRef s) :: xs ->
                let search = List.findi st.locals ~f:(fun _i v -> 
                    match v with | GcRef s' when String.equal s s' -> true | _ -> false) 
                in
                (match search with
                | Some (idx, _) -> W.LocalGet (to_wasm_imm idx) :: asm xs { st with stack = (GcRef s) :: st.stack } wf
                | None -> 
                    (match get_addr s with
                                    | [] -> asm xs st wf (* already on stack *)
                                    | is -> is @ asm xs { st with stack = (GcRef s) :: st.stack } wf))
        | (GetFunc idx) :: xs -> (W.RefFunc (to_wasm_imm idx)) :: asm xs { st with stack = (GcRef "func") :: st.stack } wf
        | (InitAddr (s, stack_tp)) :: xs ->
                (match st.stack with
                 | (Unit _) :: rest -> asm xs st wf
                 | (InjTag s') :: [] when String.equal s s' -> 
                        (* special case where s is a bool and only thing on the stack *)
                        let (i1, st') = put_local st in
                        wf.nlocals := Int.max !(wf.nlocals) (List.length st'.locals);
                        i1 @ asm xs st' wf
                 | (GcRef s') :: rest when String.equal s s' -> 
                        let (i1, st') = put_local st in
                        wf.nlocals := Int.max !(wf.nlocals) (List.length st'.locals);
                        wf.local_tps := ((List.length st'.locals - 1), stack_tp) :: !(wf.local_tps);
                        i1 @ asm xs st' wf
                 | (Addr s') :: rest when String.equal s s' -> 
                        (* don't need to realloc, but might need local *)
                        let (i1, st') = put_local st in
                        wf.nlocals := Int.max !(wf.nlocals) (List.length st'.locals);
                        i1 @ asm xs st' wf
                 | _ :: _ :: rest when not @@ Set.mem !(wf.read_in_func) s ->
                        let i1 = W.Call (fn_idxs.alloc |> to_wasm_imm) in
                        let stack = (Addr s) :: rest in
                        let (i2, st') = put_local { st with stack } in
                        wf.nlocals := Int.max !(wf.nlocals) (List.length st'.locals);
                        i1 :: i2 @ asm xs st' wf
                 | _ :: _ :: rest when Set.mem !(wf.read_in_func) s ->
                        let (i1, st') = put_local st in
                        let (i2, st'') = put_local st' in
                        wf.nlocals := Int.max !(wf.nlocals) (List.length st''.locals);
                        i1 @ i2 @ asm xs st'' wf)
        | (InitClo (tp_idx, func_idx, ncaps, s)) :: xs ->
             let stack = List.drop st.stack (ncaps + 1) in
             let snew = [W.StructNew ((to_wasm_imm tp_idx), W.Explicit)] in (* i think the Explicit has to do with up/downcasting *)
             snew @ asm xs { st with stack = (GcRef s) :: stack } wf
        | (Call ("_add_", d, _ps)) :: xs ->
             let _ :: _ :: rest = st.stack in
             (W.Binary (I32 Add)) :: asm xs { st with stack = (Addr d) :: rest } wf
        | (Call ("_sub_", d, _ps)) :: xs ->
             let _ :: _ :: rest = st.stack in
             (W.Binary (I32 Sub)) :: asm xs { st with stack = (Addr d) :: rest } wf
        | (Call ("_eqz_", d, _ps)) :: xs ->
             let graph = st.addrs |> add_rel ~addr:d ~rel:(InjVal d) in
             let _ :: rest = st.stack in
             (W.Test (I32 Eqz)) :: asm xs { st with stack = (InjTag d) :: rest; addrs = graph } wf
        | (Call ("_lt_", d, _ps)) :: xs ->
             let graph = st.addrs |> add_rel ~addr:d ~rel:(InjVal d) in
             let _ :: _ :: rest = st.stack in
             (W.Compare (I32 LtS)) :: asm xs { st with stack = (InjTag d) :: rest; addrs = graph } wf
        | (Call (p, d, _ps)) :: xs -> (* parms should already be pushed *)
                let (idx, proc) = List.findi_exn env.proc_ls ~f:(fun _i p' -> String.equal p p') in
                let ((_d, proc_dest_tp), proc_parms, _) = Map.find_exn env.procs p in
                let stack =
                    let popn = List.length proc_parms in
                    let rest = List.drop st.stack popn in
                    match proc_dest_tp with
                    | Arrow (_, _) -> (GcRef d) :: rest
                    | _ -> (Addr d) :: rest
                in
                begin if (String.equal d wf.ret_dest) && not (String.equal "main" wf.name) then
                    if List.length xs > 0 then raise TypeError else
                    [W.ReturnCall (to_wasm_imm (idx + 1 + 1 + fn_idxs.free))]
                else
                    (W.Call (to_wasm_imm (idx + 1 + 1 + fn_idxs.free))) :: asm xs { st with stack } wf
                end
         | (InvokeClo (arg, dst, clo_tp_idx)) :: xs ->
                 let f1 :: f2 :: a :: rest = st.stack in
                 let clo_tp = List.nth_exn (!T.type_decls) clo_tp_idx in
                 let ret_stackval =
                     let CloType ct = clo_tp in
                     match ct.ret with
                     | I32 -> Addr dst
                     | Clo _ -> GcRef dst
                 in
                 let struct_get = W.StructGet (to_wasm_imm clo_tp_idx, to_wasm_imm 0, None) in
                 let call = W.CallRef (to_wasm_imm (clo_tp_idx + 1)) in
                 struct_get :: call :: asm xs { st with stack = ret_stackval :: rest } wf
         | (Switch (s, need_inj, ls)) :: xs ->
                 (* TODO: cleanup, optimize to bulk load *)
                 let (dealloc_instrs, st) = match st.stack with
                 | (InjTag t) :: (InjData t') :: _rest when String.equal s t && String.equal s t' ->
                         let (i1, st') = put_local st in
                         let (i2, st'') = put_local st' in
                         wf.nlocals := Int.max !(wf.nlocals) (List.length st'.locals);
                         (i1 @ i2, st'')
                 | (InjTag t) :: _rest when String.equal s t && (not need_inj) ->
                         let (i, st') = put_local st in
                         wf.nlocals := Int.max !(wf.nlocals) (List.length st'.locals);
                         (i, st')
                 | _ when need_inj ->
                         let has_tag_local = Option.is_some @@ List.find st.locals ~f:(fun v -> is_tag s v) in
                         let has_inj_local = Option.is_some @@ List.find st.locals ~f:(fun v -> is_inj s st v) in
                         if not (has_tag_local && has_inj_local) then (
                             let get_inj = get_inj s in
                             let (put_inj, st') = put_local { st with stack = (InjData s) :: st.stack } in
                             let get_tag = get_tag s in
                             let (put_tag, st'') = put_local { st' with stack = (InjTag s) :: st'.stack } in
                             let get_addr = get_addr s in
                             let free = [W.Call (to_wasm_imm fn_idxs.free)] in
                             wf.nlocals := Int.max !(wf.nlocals) (List.length st''.locals);
                             (get_inj @ put_inj @ get_tag @ put_tag @ get_addr @ free, st'')
                             (*(get_inj @ put_inj @ get_tag @ put_tag, st'')*)
                         ) else (
                             ([], st)
                        )
                 | _ -> (* don't need inj *)
                         let has_tag_local = Option.is_some @@ List.find st.locals ~f:(fun v -> is_tag s v) in
                         if not has_tag_local then
                             let get_tag = get_tag s in
                             let (put_tag, st') = put_local { st with stack = (InjTag s) :: st.stack } in
                             wf.nlocals := Int.max !(wf.nlocals) (List.length st'.locals);
                             (get_tag @ put_tag, st')
                         else
                             ([], st)
                 in
                 let get_tag_instrs =
                     let (i, _) = List.findi_exn st.locals ~f:(fun _i v -> is_tag s v) in
                     [W.LocalGet (to_wasm_imm i)]
                 in
                 let n = List.length ls in
                 (*let bt = W.ValBlockType (Some (WT.NumT I32T)) in *)
                 let bt = W.ValBlockType None in
                 (*let bt = W.VarBlockType (type_idxs.i32_to_i32 |> Int32.of_int_exn |> to_region) in*)
                 let br_table = 
                     W.Block (bt, 
                         (W.Const (to_wasm_int 0 |> to_region) :: get_tag_instrs) @ [W.BrTable (List.(range 0 n |> map ~f:to_wasm_imm), (to_wasm_imm 0))] 
                         |> List.map ~f:to_region
                    )
                 in
                 let inner_st = match get_tag_instrs with
                 | [] -> { st with stack = List.tl_exn st.stack } 
                 | _ -> st (* tag was not there, so it was pushed, and then popped by br table *)
                 in
                 let switch = List.fold ls ~init:br_table ~f:(fun acc (i, is) -> 
                     W.Block (bt, (acc :: asm is inner_st wf) |> List.map ~f:to_region))
                 in
                 if not ((List.length xs) = 0) then raise Todo;
                 dealloc_instrs @ [switch]
    in

    let rec get_captures (cmd : S.cmd) : string list =
        let bindings (pat : S.pat) = match pat with
        | UnitPat -> []
        | InjPat (_, v) -> [v]
        | PairPat (l, r) -> [l; r]
        in
        match cmd with
        | Id (_, r) -> [r]
        | Write (_, p) -> bindings p
        | WriteCont (_, cs) -> List.concat_map cs ~f:(fun (p, b) -> 
                let bs = bindings p in 
                get_captures b |> List.filter ~f:(fun c -> not @@ List.exists bs ~f:(String.equal c)))
        | Read (v, cs) -> v :: List.concat_map cs ~f:(fun (p, b) ->
                let bs = bindings p in 
                get_captures b |> List.filter ~f:(fun c -> not @@ List.exists bs ~f:(String.equal c)))
        | Call (_, _, args) -> args
        | Cut (v, _, l, r) -> get_captures l @ (get_captures r |> List.filter ~f:(fun c -> not (String.equal c v)))

    in

    let rec compile_dest ~(cmd : S.cmd) ~(dest : (string * S.tp)) ~(vars : var_env) (wasm_func : wasm_func_env) : (macro_inst list) * wasm_func_env = 
        match cmd with
        | S.Cut (v, t, l, r) ->
              (* TODO: typecheck *)
              wasm_func.cut_in_func := Set.add !(wasm_func.cut_in_func) v;
              let (is, e) = compile_dest ~cmd:l ~dest:(v, t) ~vars wasm_func in
              let new_vars = Map.set vars ~key:v ~data:t in
              let (i, e') = compile_dest ~cmd:r ~dest ~vars:new_vars e in
              let stack_tp = T.st_of_sax_tp t in
              ((is @ (InitAddr (v, stack_tp) :: i)), e')
        (*| S.Call (("_eqz_" | "_lt_") as p, d, ps) ->*)
        (*      let gets = List.map ps ~f:(fun v -> GetAddr v) in*)
        (*      ((PushUnit d) :: gets @ [Call (p, d, ps)], wasm_func)*)
        | S.Call (p, d, []) when String.is_prefix p ~prefix:"_const_" ->
              let n = String.drop_prefix p (String.length "_const_") |> int_of_string in
              ([PushInt (n, d)], wasm_func)
        | S.Call (p, d, ps) ->
              let (dst, args) = match Map.find env.procs p with
              | Some (dst, args, _) -> (dst, args)
              | None -> (match p with
                         | "_eqz_" -> (("d", S.TpName "bool"), [("a", S.TpName "int")])
                         | "_lt_" -> (("d", S.TpName "bool"), [("a", S.TpName "int"); ("b", S.TpName "int")])
                         | ("_add_" | "_sub_") -> (("d", S.TpName "int"), [("a", S.TpName "int"); ("b", S.TpName "int")]))
              in
              if not (List.length args = List.length ps) then raise TypeError;
              let gets = List.map ps ~f:(fun v -> GetAddr v) in
              (gets @ [Call (p, d, ps)], wasm_func)
        | S.ReadClo (f, PairPat (a, d)) ->
              let clo_tp = Map.find_exn vars f |> fix in
              let clo_stack_tp = match T.st_of_sax_tp clo_tp with
              | Clo ct -> ct
              | _ -> raise TypeError
              in
              let clo_tp_idx = T.typ_idx (CloType clo_stack_tp) in
              ([GetRef f; GetAddr a; GetRef f; InvokeClo (a, d, clo_tp_idx)], wasm_func)
        | S.Read (v, bs) ->
              wasm_func.read_in_func := Set.add !(wasm_func.read_in_func) v;
              let subj_tp = Map.find_exn vars v |> fix in
              (match subj_tp with
              | One -> 
                      (* TODO: typecheck *)
                      let [(UnitPat, b)] = bs in
                      let inner_env = Map.remove vars v in
                      let (is, _e) = compile_dest ~cmd:b ~dest:dest ~vars:inner_env wasm_func in
                      (is, wasm_func)
              | Times (lt, rt) -> 
                      (match bs with
                       | [PairPat (lv, rv), b] -> 
                               let inner_env = vars |> Map.set ~key:lv ~data:lt |> Map.set ~key:rv ~data:rt in
                               let (is, _e) = compile_dest ~cmd:b ~dest:dest ~vars:inner_env wasm_func in
                               (* TODO: *)
                               (((DerefPair v) :: AliasPair (v, (lv, rv)) :: is), wasm_func)
                       | _ -> raise BadMatch)
              | Plus ls -> 
                      let bs = List.map bs ~f:(function (InjPat (l, v), b) -> (l, (v, b)) | _ -> raise BadMatch) in
                      let cases = List.map ls ~f:(fun (l, t) -> (l, List.Assoc.find_exn bs l ~equal:String.equal)) in
                      let case_instrs = List.mapi cases ~f:(fun i (l, (v', b)) -> 
                          let inner_env = Map.set vars ~key:v' ~data:(List.Assoc.find_exn ls l ~equal:String.equal) in
                          let (is, _e) = compile_dest ~cmd:b ~dest:dest ~vars:inner_env wasm_func in
                          (i, (AliasInj (v, v')) :: is))
                      in
                      let need_inj =
                          let tp = Map.find_exn vars v in
                          let Plus ls = fix tp in
                          List.exists ls ~f:(fun (_l, t) -> match t with | One -> false | _ -> true)
                      in
                      ([Switch (v, need_inj, case_instrs)], wasm_func))
        | _ ->
            let (dest_var, dest_tp) = dest in
            let dest_tp' = fix dest_tp in
            (* TODO: typecheck *)
            (match dest_tp' with
            | One -> (match cmd with
                      | S.Write (v, S.UnitPat) when String.equal dest_var v -> ([PushUnit v], wasm_func)
                      | S.Id (l, r) when String.equal dest_var l -> ([PushUnit l], wasm_func)
                      | _ -> raise TypeError)
            | Times (lt, rt) -> (match cmd with
                                 | S.Write (v, S.PairPat (l, r)) when String.equal dest_var v ->
                                         ([GetAddr l; GetAddr r; AliasPair (v, (l, r))], wasm_func)
                                 | S.Id (l, r) when String.equal dest_var l -> ([GetAddr r; Move (r, dest_var)], wasm_func)
                                 | _ -> raise TypeError)
            | Plus ls -> (match cmd with
                          | S.Write (v, S.InjPat (l, v')) when String.equal dest_var v && List.exists ls ~f:(fun (l', _) -> String.equal l l') ->
                                let (tag_idx, (_, tag_tp)) = List.findi ls ~f:(fun i (l', _) -> String.equal l l') |> Option.value_exn in
                                let push_idx = PushTag (tag_idx, v) in
                                (match fix tag_tp with
                                 | One -> ([GetUnit v'; push_idx], wasm_func)
                                 |_ -> ([GetAddr v'; push_idx], wasm_func))
                          | S.Id (l, r) when String.equal dest_var l -> ([GetAddr r; Move (r, dest_var)], wasm_func)
                          | _ -> raise TypeError)
            | Arrow (itp, otp) -> (match cmd with
                               | S.WriteCont (v, [(S.PairPat (i, o), body)]) ->
                                       (* 1. generate a top level def from body, and struct for args
                                          2. get captures (don't bother reading yet cause it's a pain)
                                          3. put ref on stack *)
                                       let fn_idx = 
                                           List.length !(env.clo_funcs) + 
                                           List.length (env.proc_ls) + fn_idxs.free + 2 
                                       in
                                       let fn_idx = match otp with
                                       | Arrow (_, _) -> fn_idx + 1
                                       | _ -> fn_idx
                                       in
                                       let captures = 
                                           get_captures body 
                                           |> List.filter ~f:(fun c -> not (String.equal c i))
                                           |> List.map ~f:(fun c -> (c, Map.find_exn vars c))
                                       in
                                       let ncaptures = List.length captures in 
                                       let inner_wf = empty_func_env "clo" o in
                                       let (body, wf) = compile_dest ~cmd:body ~dest:(o, otp) ~vars:(Map.set vars ~key:i ~data:itp) inner_wf in
                                       let clo = ((o, otp), (i, itp), captures, wf, body) in
                                       env.clo_funcs := !(env.clo_funcs) @ [clo];
                                       let clo_tp =
                                           let capture_tps = List.map captures ~f:(Fn.compose T.st_of_sax_tp snd) in
                                           let inp = T.st_of_sax_tp itp in
                                           let ret = T.st_of_sax_tp otp in
                                           T.{ capture_tps; inp; ret }
                                       in
                                       let clo_tp_idx = T.typ_idx (CloType clo_tp) in
                                       let gets = List.map captures ~f:(fun (c, _) -> GetAddr c) in
                                       let push_func_idx = GetFunc fn_idx in
                                       let init = [InitClo (clo_tp_idx, fn_idx, ncaptures, v)] in
                                       (push_func_idx :: gets @ init, wasm_func)
                               | S.Id (l, r) when String.equal dest_var l -> ([GetAddr r; Move (r, dest_var)], wasm_func))
            | TpName "int" -> (match cmd with
                               | S.Id (l, r) when String.equal dest_var l -> ([GetAddr r; Move (r, dest_var)], wasm_func)
                               | _ -> raise TypeError)
            | _ -> raise Todo)
    in

    (compile_dest, asm)
    (*| Times (lt, rt) ->*)
