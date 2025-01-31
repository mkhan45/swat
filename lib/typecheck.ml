open Ast
open Core

exception UnunificationException of string
exception Undefined

type missing_write = { expected: (varname * tp); got: (varname * tp) }
exception MissingWrite of missing_write

exception MatchError

type type_mismatch = { var: varname; wrote: tp; read: tp }

module Substs = struct
  include Map.Make(String)
end

let type_nat = Plus [("zero", One); ("succ", TpName "nat")]
let type_unary = Plus [("zero", One); ("succ", TpName "unary")]
let type_pos = Plus [("succ", TpName "nat")]


let rec type_unify (type1 : tp) (type2 : tp) : (string * tp) list =
  match (type1, type2) with
  | (One, One) -> []
  | (Times (x1, x2), Times (y1, y2)) -> (type_unify x1 x2) @ (type_unify y1 y2)
  | (Plus xs, Plus ys) -> sum_list xs ys 
  | (TpName v, t) | (t, TpName v) -> [(v, t)]
  | _ -> raise (UnunificationException "failed to unify")

and sum_list (xs : (label * tp) list) (ys : (label * tp) list) : (string * tp) list =
  let xmap = Map.of_alist_exn (module String) xs in
  let ymap = Map.of_alist_exn (module String) ys in 
  if Map.length xmap <> Map.length ymap then
    raise (UnunificationException "nonequal map list")
  else
    let keys = Map.keys xmap in
      List.fold keys ~init:[] ~f:(fun acc label -> let x = Map.find_exn xmap label in
                                                   let y = Map.find_exn ymap label in
                                                     type_unify x y @ acc)

let type_equals (type1 : tp) (type2 : tp) : bool =
  try
    let substs = Substs.empty in
    let partial_substs = type_unify type1 type2 in
    let rec go ts substs =
      match ts with
      | [] -> substs
      | (v, t)::ts' -> (match Map.find substs v with
                              | None -> go ts' (Map.set substs ~key:v ~data:t)
                              | Some t' -> let new_substs = type_unify t' t in
                                             go (new_substs @ ts') substs)
    in
      go partial_substs substs |> const true
  with
  | UnunificationException _ -> false

let%test _ = type_equals type_nat type_unary
let%test _ = not (type_equals type_nat type_pos)

module Hamburger = struct
  type t = (string, tp, String.comparator_witness) Map.t
  type res = TypeMismatch of type_mismatch | NotFound of varname | Unused of t | SingleUnused of varname
  exception TypeMismatch of res

  let _smush (_left : t) (_right : t) : t option = raise Undefined 

  let empty : t = Map.empty (module String)

  let rec get env v = match Map.find env v with
                      | Some r -> r
                      | None -> raise (TypeMismatch (NotFound v))
  (* let rec get env v = match Map.find env v with *)
  (*                   | Some (TpName n) -> get env n *)
  (*                   | Some r -> r *)
  (*                   | None -> *) 
  (*                           Printf.printf "1: Not found %s\n" v; *)
  (*                           raise (TypeMismatch (NotFound v)) *)

  let print h = Map.iteri h ~f:(fun ~key ~data -> Printf.printf "%s: %s\n" key (Print.pp_tp data))

  let print_ls h = List.iter h ~f:(fun (key, data) -> Printf.printf "%s: %s\n" key (Print.pp_tp data))

  let rec type_subtype (env : t) (type1 : tp) (type2 : tp) : bool =
    match (type1, type2) with
    | (One, One) -> true
    | (Times (x1, x2), Times (y1, y2)) -> type_subtype env x1 y1 && type_subtype env x2 y2
    | (Plus xs, Plus ys) ->
      let xmap = Map.of_alist_exn (module String) xs in
      let ymap = Map.of_alist_exn (module String) ys in 
      let keys = Map.keys xmap in
          List.for_all keys ~f:(fun label -> let x = Map.find_exn xmap label in
                                              match Map.find ymap label with
                                              | Some y -> type_subtype env x y
                                              | None -> false)
    | (TpName x, TpName y) when String.equal x y -> true
    | (TpName x, y) -> type_subtype env (get env x) y
    | (x, TpName y) -> type_subtype env x (get env y)
    | _ -> false

  (* and check_complete (typenames : t) (env : t) (reads : (varname * tp) list) : res = match reads with *)
  (* | [] -> Unused env *)
  (* | (read_var, read_tp) :: rest -> begin match Map.find env read_var with *)
  (*                                  | Some write_tp when type_subtype typenames read_tp write_tp -> check_complete typenames (Map.remove env read_var) rest *)
  (*                                  | Some write_tp -> (TypeMismatch { var = read_var; wrote = write_tp; read = read_tp }) *)
  (*                                  | None -> (NotFound read_var) *)
  (*                                  end *)

  let rec interfere (typenames : t) (env : t) (reads : (varname * tp) list) : (t * ((varname * tp) list)) = match reads with
  | [] -> (env, [])
  | (read_var, read_tp) :: rest -> begin match Map.find env read_var with
                                   | Some write_tp when type_subtype typenames read_tp write_tp -> interfere typenames (Map.remove env read_var) rest
                                   | Some write_tp -> raise (TypeMismatch (TypeMismatch { var = read_var; wrote = write_tp; read = read_tp }))
                                   | None ->
                                           let (res_env, res_reads) = interfere typenames env rest in
                                           (res_env, (read_var, read_tp) :: res_reads)
                                   end
end

let type_subtype = Hamburger.type_subtype

let%test _ = type_subtype (Hamburger.empty) type_pos type_nat
let%test _ = not (type_subtype Hamburger.empty type_nat type_pos)


module Procs = struct
    type t = (string, parm * parm list * cmd, String.comparator_witness) Map.t

    let _args_subtype (_proc_name : string) (_args : varname list) ~(_env: Hamburger.t) = raise Undefined
end

module Labels = struct
    type t = (string, tp, String.comparator_witness) Map.t

    let empty = Map.empty (module String)
end

(*let typecheck (env : Hamburger.t) (com : cmd) (var, vartp) : Hamburger.t option = None*)

type inferred_tp = { write : (varname * tp); read : (varname * tp) list }
let print_infer infer = 
    Printf.printf "---\nwrite = %s : %s\nread:\n" (fst infer.write) (Print.pp_tp (snd infer.write));
    Hamburger.print_ls infer.read;
    Printf.printf "---\n";;

let ensure_used (typenames : Hamburger.t) (read : (varname * tp) list) (v : varname) (t : tp) : ((varname * tp) list) =
    match List.Assoc.find read v ~equal:String.equal with
    | Some rt when type_subtype typenames t rt -> List.Assoc.remove read v ~equal:String.equal
    | Some rt -> 
            Printf.printf "var: %s\nwrote:%s\nread:%s\n" v (Print.pp_tp t) (Print.pp_tp rt);
            raise Hamburger.(TypeMismatch (TypeMismatch { var = v; wrote = t; read = rt }))
    | None -> raise Hamburger.(TypeMismatch (SingleUnused v)) 

let typecheck (typenames : Hamburger.t) (procs : Procs.t) (labels: Labels.t) : bool =
    let rec infer (com : cmd) (tps: Hamburger.t) : inferred_tp = 
        let (!!) v = match Hamburger.get tps v with
                   | TpName n -> Map.find_exn typenames n
                   | t -> t
        in
        let ensure_used v t inf = { inf with read = ensure_used typenames inf.read v t } in
        match com with
        | Id (into, from) -> { write = (into, !!from); read = [(from, !!from)] }
        | Read (from, cases) -> 
                begin match (!!from, cases) with
                | (One, [(UnitPat, com)]) -> let i = infer com tps in { i with read = (from, One)::i.read }
                | (Times (lt, rt), [(PairPat (lv, rv), com)]) -> 
                        let tps = tps |> Map.set ~key:lv ~data:lt |> Map.set ~key:rv ~data:rt in
                        let body_infer = infer com tps in
                        { body_infer with read = (from, !!from) :: body_infer.read } |> ensure_used lv lt |> ensure_used rv rt
                | (Plus tp_cases, match_cases) when (List.length tp_cases) = (List.length match_cases) -> 
                        (* for label in match case, new env = bind var to type, infer, ensure all reads/writes are the same *)
                        let cases = List.map match_cases ~f:(fun (p, com) -> match p with
                                                                             | InjPat (l, v) -> 
                                                                                     let tps = Map.set tps ~key:v ~data:(Map.find_exn labels l) in
                                                                                     let i = infer com tps  in
                                                                                     i |> ensure_used v (Map.find_exn labels l)
                                                                             | _ -> raise Undefined (* should be impossible by grammar *))
                        in
                        let case_hd = List.hd_exn cases in (* all should be the same *)
                        { case_hd with read = (from, !!from) :: case_hd.read }
                        (* if List.for_all cases ~f:(phys_equal case_hd) then case_hd else raise MatchError *)
                | _ -> 
                        raise MatchError
                end
        | Write (into, tipe_pat) ->
                begin match tipe_pat with
                | UnitPat -> 
                        { write = (into, One); read = [] }
                | PairPat (left, right) -> 
                        { write = (into, Times (!!left, !!right)); read = [(left, !!left); (right, !!right)] }
                | InjPat (l, v) -> 
                        let t = Plus [(l, !!v)] in { write = (into, t); read = [(v, !!v)] }
                end
        | Cut (new_v, new_t, left, right) ->
                let l_infer = infer left tps in
                let (l_write_var, l_write_tp) = l_infer.write in
                if not ((String.equal new_v l_write_var) && (type_subtype typenames l_write_tp new_t)) 
                then 
                    raise (MissingWrite { expected = (new_v, new_t); got = l_infer.write })
                else begin
                    (* let (new_env, remaining_reads) = Hamburger.interfere typenames env l_infer.read in *)
                    let tps = tps |> Map.set ~key:new_v ~data:new_t in
                    let r_infer = infer right tps |> ensure_used new_v new_t in
                    { r_infer with read = l_infer.read @ r_infer.read }
                    (* let (final_env, final_reads) = Hamburger.interfere typenames new_env (remaining_reads @ r_infer.read) in *)
                    (* { r_infer with read = final_reads } *)
                end
        | Call (proc_name, dest, args) -> 
                (* TODO: actually check args, dest type against proc, can use Procs helper function *)
                let (proc_dest, proc_args, _proc_body) = Map.find_exn procs proc_name in
                let (_, proc_dest_tp) = proc_dest in
                let read = List.zip_exn args proc_args |> List.map ~f:(fun (v, (_, t)) -> (v, t)) in
                { write = (dest, proc_dest_tp); read }
    in
    Map.for_alli procs ~f:(fun ~key ~data ->
        let ((dest_var, dest_tp), args, body) = data in
        let tps = List.fold_left args ~init:Hamburger.empty ~f:(fun env (v, t) -> Map.set env ~key:v ~data:t) in
        let { write = (write_var, write_tp); read } = infer body tps in
        (*print_infer res;*)
        let dest_tp = match dest_tp with
        | TpName n -> Hamburger.get typenames n
        | _ -> dest_tp
        in
        (List.length read = List.length args) && (String.equal write_var dest_var) && (type_subtype typenames write_tp dest_tp)
    )
