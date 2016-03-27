open Core.Std
module EE = Llvm_executionengine
module T = ANSITerminal

let dump_llvm = ref false

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

let flush () = Out_channel.flush Out_channel.stdout

let build_file (path : string) =
  (* Lex the input into tokens *)
  let lexbuf = Lexing.from_channel (open_in path) in
  let ast = parse_buf My_parser.prog lexbuf in
  let m = Codegen.codegen ast in

  (* Write out the object file and compile it to a binary *)
  let obj_file = (Util.write_to_temp_file "") in
  Codegen.emit_object m obj_file;
  let output_path = (Filename.chop_extension (Filename.basename path)) in
  let err = Sys.command ("gcc " ^ obj_file ^ " -o " ^ output_path) in
  if err <> 0 then failwith "Compilation failed"
  else ()

let start_jit () =
  let anon_counter = ref 0 in
  let jit_module = Llvm.create_module (Llvm.global_context ()) "jit_module" in
  let execution_engine = EE.ExecutionEngine.create jit_module in
  In_channel.iter_lines In_channel.stdin ~f:(fun line ->
    let lexbuf = Lexing.from_string line in
    match My_lexer.read (Lexing.from_string line) with
    | My_parser.DEF ->
      let ast = parse_buf My_parser.prog lexbuf in
      ignore (Codegen.codegen ast);
    | My_parser.EXTERN ->
      let ast = parse_buf My_parser.extern lexbuf in
      ignore (Codegen.codegen_proto ast);
    | _ ->
      let ast = parse_buf My_parser.expr_eof lexbuf in
      let anon_id = "anon" ^ (Int.to_string_hum !anon_counter) in
      incr anon_counter;
      let prog = Ast.Program [Ast.Function (Ast.Prototype (anon_id, []), ast)] in
      let m = Codegen.codegen prog in
      EE.ExecutionEngine.add_module m execution_engine;
      let fn = Option.value_exn (Llvm.lookup_function anon_id m) in
      let result =
        EE.GenericValue.as_int
          (EE.ExecutionEngine.run_function fn [||] execution_engine)
      in
      T.printf [T.red; T.Bold] "Result: "; printf "%d\n" result;
      flush ()
  )

let () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"y0 compiler"
    [%map_open
      let run_jit = flag "-jit" no_arg ~doc:"Run JIT interpreter"
      and dump_llvm' = flag "-v" no_arg ~doc: "Print LLVM"
      and path = anon (maybe ("path" %: string))
      in fun () ->
        dump_llvm := dump_llvm';
        Codegen.init();
        if run_jit then start_jit ()
        else build_file (Option.value_exn path)
    ]
  |> Command.run
