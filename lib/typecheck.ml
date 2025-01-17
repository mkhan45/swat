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
      | (TpName n, t)::ts' -> (match Map.find substs n with
                              | None -> go ts' (Map.set substs ~key:n ~data:t)
                              | Some t' -> let new_substs = type_unify t' t in
                                             go (new_substs @ ts') substs)
      | _ -> raise (UnunificationException "false")
    in
      go partial_substs substs |> const true
  with
  | UnunificationException _ -> false


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
  | _ -> false
