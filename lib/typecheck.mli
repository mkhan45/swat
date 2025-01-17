open Ast

(* typechecker *)
(* val typecheck : varname set -> cmd -> varname -> (varname set, string) result *)

(* type comparison *)
val type_equals: tp -> tp -> bool
val type_subtype: tp -> tp -> bool
val type_unify: tp -> tp -> (tp * tp) list
(*
module type Hamburger = sig
  include Set.S with type Elt.t = varname

  (* functions *)
end
*)
