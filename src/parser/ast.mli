open Token

type ident = string

type operator =
  | Add
  | Subtract
  | Multiply
  | Divide

type expr =
  | Number of int
  | Variable of ident
  | Binary of operator * expr * expr
  | Call of ident * expr list

type proto = Prototype of ident * (ident list)
type func = Function of proto * expr
type program = Program of func list
