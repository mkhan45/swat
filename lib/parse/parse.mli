(** Parsing

    Gluing together the pieces produced by ocamllex and menhir. *)

(** [parse_policy filename] parses a {!Ast.policy} from the contents of the file

    @raise Error_msg.Error in case of lexing or parsing error *)
val parse : string -> Ast.env

