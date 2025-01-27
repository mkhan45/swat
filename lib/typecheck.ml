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

let rec type_subtype (type1 : tp) (type2 : tp) : bool =
  match (type1, type2) with
  | (One, One) -> true
  | (Times (x1, x2), Times (y1, y2)) -> type_subtype x1 y1 && type_subtype x2 y2
  | (Plus xs, Plus ys) ->
    let xmap = Map.of_alist_exn (module String) xs in
    let ymap = Map.of_alist_exn (module String) ys in 
    let keys = Map.keys xmap in
        List.for_all keys ~f:(fun label -> let x = Map.find_exn xmap label in
                                            match Map.find ymap label with
                                            | Some y -> type_subtype x y
                                            | None -> false)
  | (TpName x, TpName y) -> String.equal x y
  | _ -> false

let%test _ = type_subtype type_pos type_nat
let%test _ = not (type_subtype type_nat type_pos)

module Hamburger = struct
  type t = (string, tp, String.comparator_witness) Map.t
  type error = TypeMismatch of type_mismatch | NotFound of varname | Unused of t
  exception TypeMismatch of error

  let _smush (_left : t) (_right : t) : t option = raise Undefined 

  let rec check_complete (env : t) (reads : (varname * tp) list) : error option = match reads with
  | [] when Map.is_empty env -> None
  | [] (* otherwise *) -> Some (Unused env)
  | (read_var, read_tp) :: rest -> begin match Map.find env read_var with
                                   | Some write_tp when type_subtype read_tp write_tp -> check_complete (Map.remove env read_var) rest
                                   | Some write_tp -> Some (TypeMismatch { var = read_var; wrote = write_tp; read = read_tp })
                                   | None -> Some (NotFound read_var)
                                   end
end

module Procs = struct
    type t = (string, tp list, String.comparator_witness) Map.t

    let _args_subtype (_proc_name : string) (_args : varname list) ~(_env: Hamburger.t) = raise Undefined
end

(*let typecheck (env : Hamburger.t) (com : cmd) (var, vartp) : Hamburger.t option = None*)

type inferred_tp = { write : (varname * tp); read : (varname * tp) list }

let typecheck (_procs : Procs.t) (_dest : tp) (_body : cmd) : bool =
    let rec infer (com : cmd) (env : Hamburger.t) : inferred_tp = 
        let ( !! ) v = match Map.find env v with
                       | Some r -> r
                       | None -> raise Hamburger.(TypeMismatch (NotFound v))
        in
        match com with
        | Id (into, from) -> { write = (into, !!from); read = [(from, !!from)] }
        | Read (from, cases) -> 
                let new_env = Map.remove env from in
                begin match (!!from, cases) with
                | (One, [(UnitPat, com)]) -> infer com new_env
                | (Times (lt, rt), [(PairPat (lv, rv), com)]) -> infer com (new_env |> Map.set ~key:lv ~data:lt |> Map.set ~key:rv ~data:rt)
                | (Plus tp_cases, match_cases) when phys_equal (List.length tp_cases) (List.length match_cases) -> 
                        (* TODO: instead of zipping, map over match cases, all should be injpat, then check against type env *)
                        (*let cases = List.zip_exn tp_cases match_cases |> List.map ~f:(fun ((_l, t), (p, com)) -> infer com (bind new_env t p)) in*)
                        raise Undefined
                | _ -> raise MatchError
                end
        | Write (into, tipe_pat) ->
                begin match tipe_pat with
                | UnitPat -> { write = (into, One); read = [] }
                | PairPat (left, right) -> { write = (into, Times (!!left, !!right)); read = [(left, !!left); (right, !!right)] }
                | InjPat (l, v) -> let t = Plus [(l, !!v)] in { write = (into, t); read = [(v, !!v)] }
                end
        | Cut (new_v, new_t, left, right) ->
                let l_infer = infer left env in
                let (l_write_var, l_write_tp) = l_infer.write in
                if not ((String.equal new_v l_write_var) && (type_subtype l_write_tp new_t)) 
                then 
                    raise (MissingWrite { expected = (new_v, new_t); got = l_infer.write })
                else begin match Hamburger.check_complete env l_infer.read with
                  | Some err -> raise (Hamburger.TypeMismatch err)
                  | None -> infer right (Map.set env ~key:new_v ~data:new_t)
                end
        | Call (_proc, dest, args) -> 
                (* TODO: actually check args, dest type against proc *)
                { write = (dest, !!dest); read = args |> List.map ~f:(fun arg -> (arg, !!arg)) }
    in 
    raise Undefined
