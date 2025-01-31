{
(** Sax Lexer *)

module S = Sax_parser

(* COPIED FROM 15-411 STARTER CODE *)
(* A record of all errors that happened. *)
let errors = Error_msg.create ()

let text = Lexing.lexeme

let from_lexbuf : Lexing.lexbuf -> Mark.src_span option =
  fun lexbuf ->
    Mark.of_positions
      (Lexing.lexeme_start_p lexbuf)
      (Lexing.lexeme_end_p lexbuf)
    |> Option.some

let error lexbuf (msg : string) =
  let src_span = from_lexbuf lexbuf in
  Error_msg.error errors src_span msg

}

let idstart = ['a'-'z' 'A'-'Z' '_']
let idchar = ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']
let ident = idstart idchar*
let label = '\'' idchar+

let digit = ['0'-'9']
let num = digit*


let ws = [' ' '\t' '\r']

rule initial = parse
  | ws+  { initial lexbuf }
  | '\n' { Lexing.new_line lexbuf;
           initial lexbuf
         }
  | "//" { comment_line lexbuf }
  | "/*" { comment_block 1 lexbuf }

  | ',' { S.COMMA }
  | ':' { S.COLON }

  | '(' { S.LPAREN }
  | ')' { S.RPAREN }
  | '{' { S.LBRACE }
  | '}' { S.RBRACE }

  | "=>" { S.RIGHTARROW }
  | '=' { S.EQUAL }
  | '|' { S.BAR }

  | '*' { S.STAR }
  | '+' { S.PLUS }

  | num as n { S.NAT (int_of_string n) }  

  | "read"   { S.READ }
  | "write"  { S.WRITE }
  | "cut"    { S.CUT }
  | "id"     { S.ID }
  | "call"   { S.CALL }
  | "type"   { S.TYPE }
  | "proc"   { S.PROC }
  | "fail"   { S.FAIL }

  | label as name { S.LABEL name }
  | ident as name { S.IDENT name }

  | eof { S.EOF }

  | _  { error lexbuf
           (Printf.sprintf "Illegal character '%s'" (text lexbuf));
         initial lexbuf
       }

and comment_line = parse
  | '\n' { Lexing.new_line lexbuf; initial lexbuf }
  | eof { initial lexbuf }
  | _ { comment_line lexbuf }

and comment_block depth = parse
  | '\n' { Lexing.new_line lexbuf; comment_block depth lexbuf }
  | "*/" { if depth = 1 then initial lexbuf else comment_block (depth-1) lexbuf }
  | "/*" { comment_block (depth+1) lexbuf }
  | _ { comment_block depth lexbuf }

{}
