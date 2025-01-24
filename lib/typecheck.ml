open Ast
open Core

exception UnunificationException of string
exception Undefined

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
  include Map.Make(String)

  type tt = tp t

  let smush (left : tt) (right : tt) : tt option = raise Undefined 
    
end

let typecheck (env : Hamburger.tt) (com : cmd) (var, vartp) : Hamburger.tt option =
  match com with
  | Id (x, y) -> (match Map.find env x with
                  | Some xtp -> (match Map.find env y with
                                 | Some ytp -> if type_subtype ytp xtp
                                                 then Some (Map.remove env y)
                                                 else None
                                 | None -> None)
                  | None -> None)
  |


