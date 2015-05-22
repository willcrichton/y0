open Core.Std
open Llvm_target

let main () =

  (* Parse the command-line arguments *)
  let input_path = ref "" in
  let dump_llvm = ref false in
  let arguments = [("--dump-llvm", Arg.Set dump_llvm, "Print generated LLVM")] in
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

  (* Lex the input into tokens *)
  let lexbuf = Lexing.from_channel input in

  (* Parse tokens into an abstract syntax tree *)
  let ast =
    try Parser.prog Lexer.read lexbuf with
    | Lexer.SyntaxError msg ->
       fprintf stderr "%a: %s\n" Lexer.print_position lexbuf msg;
       exit(-1)
    | Parser.Error ->
       fprintf stderr "%a: syntax error on token \"%s\"\n" Lexer.print_position lexbuf
         (Lexing.lexeme lexbuf);
       exit(-1)
  in

  (* Initialize LLVM target *)
  let _ = Llvm_all_backends.initialize () in
  let target = Target.by_triple (Target.default_triple()) in
  let target_machine = TargetMachine.create (Target.default_triple ()) target in

  (* Generate LLVM code from the syntax tree *)
  let llvm_module = Codegen.codegen ast target_machine in
  let _ = if !dump_llvm then Llvm.dump_module llvm_module in

  (* Write out the object file and compile it to a binary *)
  let obj_file = (Util.write_to_temp_file "") in
  let _ = TargetMachine.emit_to_file llvm_module CodeGenFileType.ObjectFile obj_file target_machine in
  let output_path = (Filename.chop_extension (Filename.basename !input_path)) in
  Sys.command ("gcc " ^ obj_file ^ " -o " ^ output_path)

let _ = main()
