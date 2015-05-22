{
  open Lexing
  open Parser
  open Core.Std

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
}


let int = '-'? ['0'-'9']+

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "def"   { DEF }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | ','     { COMMA }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '/'     { DIVIDE }
  | '*'     { MULTIPLY }
  | id      { ID (Lexing.lexeme lexbuf) }
  | _       { raise (SyntaxError ("Unexpected token: " ^ (Lexing.lexeme lexbuf) ^ " at position " ^ (string_of_int lexbuf.lex_curr_pos))) }
  | eof     { EOF }
