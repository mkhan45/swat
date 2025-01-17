open Ast
open Core

exception UnunificationException of string

module Substs = struct
  include Map.Make(String)
end


let rec type_unify (type1 : tp) (type2 : tp) : (tp * tp) list =
  match (type1, type2) with
  | (One, One) -> []
  | (Times (x1, x2), Times (y1, y2)) -> (type_unify x1 x2) @ (type_unify y1 y2)
  | (Plus xs, Plus ys) -> sum_list xs ys 
  | (TpName s, t) | (t, TpName s) -> [(TpName s, t)]
  | _ -> raise (UnunificationException "failed to unify")

and sum_list (xs : (label * tp) list) (ys : (label * tp) list) : (tp * tp) list =
  match (xs, ys) with
  | ((l1, t1)::xs', (l2, t2)::ys') when String.equal l1 l2 -> (type_unify t1 t2) @ (sum_list xs' ys')
  | ([], []) -> []
  | _ -> raise (UnunificationException "failed to sum list")

let type_equals (type1 : tp) (type2 : tp) : bool =
  try
    let substs = Substs.empty in
    let partial_substs = type_unify type1 type2 in
    let rec go ts substs =
      match ts with
      | [] -> substs
      | (TpName n, t)::ts' -> (match Map.find substs n with
                              | None -> go ts' (Map.set substs ~key:n ~data:t)
                              | Some t' -> let new_substs = type_unify t' t in
                                             go (new_substs @ ts') substs)
      | _ -> raise (UnunificationException "false")
    in
      go partial_substs substs |> const true
  with
  | UnunificationException _ -> false
