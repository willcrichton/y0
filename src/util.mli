(** [read_process cmd] executes [cmd] in the shell and returns stdout *)
val read_process : string -> string

(** [write_to_temp_file s] writes [s] to a temporary file and returns the path
    to the tempfile. *)
val write_to_temp_file : string -> string

(** [parse_buf parse_fn buf] returns the syntax tree for the buffer [buf] given
    the parse [parse_fn] function. *)
val parse_buf :
  ((Lexing.lexbuf -> My_parser.token) -> Lexing.lexbuf -> 'a)
  -> Lexing.lexbuf
  -> 'a
