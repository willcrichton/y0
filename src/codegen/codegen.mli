(** Initializes the Codegen module. Must be called before any methods below *)
val init          : unit -> unit

(** Adds a program to the module. *)
val codegen       : Ast.program -> unit

(** Adds a prototype (intended for extern defs) to the module. *)
val codegen_proto : Ast.proto -> unit

(** Writes object code for the module to the given path. *)
val emit_object   : string -> unit

(** Turns the current module into an LLVM module *)
val llvm_module   : unit -> Llvm.llmodule
