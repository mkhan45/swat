open Ast

(* type comparison *)
val type_equals: tp -> tp -> bool
val type_subtype: tp -> tp -> bool
val type_unify: tp -> tp -> (string * tp) list
