open Ast

(* type comparison *)
val type_equals: tp -> tp -> bool
val type_subtype: tp -> tp -> bool
val type_unify: tp -> tp -> (string * tp) list

module Hamburger : sig
  include Map.S

  type tt = tp t
  (* functions *)
  val smush : tt -> tt -> tt option
end

(* typechecker *)
val typecheck : Hamburger.tt -> cmd -> (varname * tp) -> Hamburger.tt option
