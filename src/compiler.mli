(** [compile path verbose] takes a [path] to a y0 file and writes an executable
    to the directory the compiler is run *)
val compile : string -> bool -> unit
