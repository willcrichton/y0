open Token

type ident = string

type operator =
  | Plus
  | Minus

type expr =
  | Number of int
  | Variable of ident
  | Binary of operator * expr * expr
  | Call of ident * expr array

type proto = Prototype of ident * ident list
type func = Function of proto * expr
type program = Program of func list
