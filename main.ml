open Core.Std
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
          pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let main () =

  (* Parse the command-line arguments *)
  let input_path = ref "" in
  let arguments = [] in
  let () =
    Arg.parse
      arguments
      (fun x -> if !input_path = "" then
                  input_path := x
                else
                  raise (Arg.Bad ("Bad arg: " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ "?")
  in
  let input = open_in !input_path in

  (* Push input through each stage of the compiler *)
  let lexbuf = Lexing.from_channel input in
  let ast =
    try Parser.prog Lexer.read lexbuf with
    | Lexer.SyntaxError msg ->
       fprintf stderr "%a: %s\n" print_position lexbuf msg;
       exit(-1)
    | Parser.Error ->
       fprintf stderr "%a: syntax error on token \"%s\"\n" print_position lexbuf (Lexing.lexeme lexbuf);
       exit(-1)
  in
  let llvm_instrs = Codegen.codegen ast in
  ()
  (* Compile the LLVM output
  let llvm_path = Util.write_to_temp_file llvm_instrs in
  let asm_path = llvm_path ^ ".S" in
  let output_path = (Filename.chop_extension Sys.argv.(1)) in
  let command = sprintf "llc %s; gcc -m64 %s -o %s" llvm_path asm_path output_path in
  printf "%s, %s" llvm_path (Util.read_process command)*)

let _ = main()
