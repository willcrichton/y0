(** [parse_buf parse_fn buf] returns the syntax tree for the buffer [buf] given
    the parse [parse_fn] function. *)
val parse_buf :
  ((Lexing.lexbuf -> My_parser.token) -> Lexing.lexbuf -> 'a)
  -> Lexing.lexbuf
  -> 'a
