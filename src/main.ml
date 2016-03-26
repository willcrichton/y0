open Core.Std
open Llvm_target
open Llvm_executionengine

let dump_llvm = ref false
let target_machine = ref None

(* Parse tokens into an abstract syntax tree *)
let parse_buf (parse : (Lexing.lexbuf -> My_parser.token) -> Lexing.lexbuf -> 'a)
      (lexbuf : Lexing.lexbuf) =
  try parse My_lexer.read lexbuf with
  | My_lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" My_lexer.print_position lexbuf msg;
    exit(1)
  | My_parser.Error ->
    fprintf stderr "%a: syntax error on token \"%s\"\n" My_lexer.print_position
      lexbuf
      (Lexing.lexeme lexbuf);
    exit(1)


let buf_to_module (lexbuf : Lexing.lexbuf) =
  let ast = parse_buf My_parser.prog lexbuf in
  let llvm_module = Codegen.codegen ast (Option.value_exn !target_machine) in
  let () = if !dump_llvm then Llvm.dump_module llvm_module in
  llvm_module

let build_file (path : string) =
  (* Lex the input into tokens *)
  let lexbuf = Lexing.from_channel (open_in path) in
  let llvm_module = buf_to_module lexbuf in

  (* Write out the object file and compile it to a binary *)
  let obj_file = (Util.write_to_temp_file "") in
  let () =
    TargetMachine.emit_to_file llvm_module CodeGenFileType.ObjectFile obj_file
      (Option.value_exn !target_machine)
  in
  let output_path = (Filename.chop_extension (Filename.basename path)) in
  let err = Sys.command ("gcc " ^ obj_file ^ " -o " ^ output_path) in
  if err <> 0 then failwith "Compilation failed"
  else ()

let start_jit () =
  let jit_module = Llvm.create_module (Llvm.global_context ()) "jit_module" in
  let execution_engine = ExecutionEngine.create jit_module in

  In_channel.iter_lines In_channel.stdin ~f:(fun line ->
    let lexbuf = Lexing.from_string line in
    match My_lexer.read lexbuf with
    | My_parser.DEF ->
      let llvm_module = buf_to_module lexbuf in
      ExecutionEngine.add_module llvm_module execution_engine
    (* | Token.Extern -> printf "extern\n" *)
    | _ ->
      let ast = parse_buf My_parser.expr lexbuf in
      (* Codegen.codegen_func (Ast.Function (Ast.Prototype ("", []), ast)); *)
      let Some fn = ExecutionEngine.find_function "test" execution_engine in
      printf "Result: %d\n"
        (GenericValue.as_int (ExecutionEngine.run_function fn [||] execution_engine))
   )

let () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"y0 compiler"
    [%map_open
      let run_jit = flag "run-jit" no_arg ~doc:"Run JIT interpreter"
      and dump_llvm' = flag "dump-llvm" no_arg ~doc: "Print LLVM"
      and path = anon (maybe ("path" %: string))
      in fun () ->
        dump_llvm := dump_llvm';
        let () = Llvm_all_backends.initialize () in
        let target = Target.by_triple (Target.default_triple()) in
        target_machine := Some (TargetMachine.create (Target.default_triple ()) target);
        if run_jit then start_jit ()
        else build_file (Option.value_exn path)
    ]
  |> Command.run
