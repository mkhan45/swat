open Core
open Ast

module Hamburger : sig
  type t = (string, tp, String.comparator_witness) Map.t
  
  val get : t -> string -> tp
  val empty : t
end

val type_subtype : Hamburger.t -> tp -> tp -> bool
val type_equals : tp -> tp -> bool

module Procs : sig
  type t = (string, parm * parm list * cmd, String.comparator_witness) Map.t
end

module Labels : sig
  type t = (string, tp, String.comparator_witness) Map.t
  val empty : t
end

val typecheck : Hamburger.t -> Procs.t -> Labels.t -> Procs.t
