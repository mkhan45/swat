(** Parsing

    Gluing together the pieces produced by ocamllex and menhir. *)

(** In order for the lexbuf to correctly track source locations
    we have to initialize the filename in the positions tracked by
    the lexbuf. *)
let initialize_lexbuf (filename : string) : Lexing.lexbuf -> unit =
  let open Lexing in
  let pos = { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  fun lexbuf ->
    lexbuf.lex_start_p <- pos;
    lexbuf.lex_curr_p <- pos
;;

let parse (filename : string) : Ast.env =
  try
    let ast =
      In_channel.with_open_text filename (fun chan ->
        let lexbuf = Lexing.from_channel chan in
        initialize_lexbuf filename lexbuf;
        try Sax_parser.prog Sax_lexer.initial lexbuf with
        | _ ->
          (* Parse error; attempt to print a helpful error message. *)
          let src_span =
            Mark.of_positions Lexing.(lexbuf.lex_start_p) Lexing.(lexbuf.lex_curr_p)
          in
          Error_msg.error Sax_lexer.errors (Some src_span) "Parse error";
          raise Error_msg.Error)
    in
    if Error_msg.has_any_errors Sax_lexer.errors
    then (
      Out_channel.output_string Out_channel.stderr "Lex error\n";
      raise Error_msg.Error)
    else ast
  with
  | Sys_error s ->
    (* Probably file not found or permissions errors. *)
    Out_channel.output_string Out_channel.stderr ("System error: " ^ s ^ "\n");
    raise Error_msg.Error
