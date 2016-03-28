open Core.Std

let parse_buf parse lexbuf =
  try parse My_lexer.read lexbuf with
  | My_lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" My_lexer.print_position lexbuf msg;
    exit(1)
  | My_parser.Error ->
    fprintf stderr "%a: syntax error on token \"%s\"\n" My_lexer.print_position
      lexbuf
      (Lexing.lexeme lexbuf);
    exit(1)
