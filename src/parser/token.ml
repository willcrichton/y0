type token =
  | Def | Extern
  | Ident of string | Number of float
  | Kwd of char
