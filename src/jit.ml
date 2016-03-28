open Core.Std
open Llvm_executionengine
module T = ANSITerminal

let prompt () =
  T.printf [T.Bold] ">>> "; Out_channel.flush Out_channel.stdout

let print_result result =
  printf "%s\n" result; prompt ()

let start verbose =
  (* Initialize LLVM's JIT *)
  let anon_counter = ref 0 in
  let jit_module = Llvm.create_module (Llvm.global_context ()) "jit_module" in
  let execution_engine = ExecutionEngine.create jit_module in

  (* Loop over stdin and JIT the input *)
  prompt ();
  In_channel.iter_lines In_channel.stdin ~f:(fun line ->
    let lexbuf = Lexing.from_string line in

    (* Lexing.lexbuf doesn't expose a peek function, so we have to mutably read
     * off a copy to emulate this *)
    match My_lexer.read (Lexing.from_string line) with
    | My_parser.DEF ->
      let ast = Util.parse_buf My_parser.prog lexbuf in
      Codegen.codegen ast;
      print_result "added function"
    | _ ->
      (* We put expressions in an "anonymous" function and call the result *)
      let ast = Util.parse_buf My_parser.expr_eof lexbuf in
      let anon_id = "_anon" ^ (Int.to_string_hum !anon_counter) in
      incr anon_counter;
      let prog = Ast.Program [Ast.Function (Ast.Prototype (anon_id, []), ast)] in
      Codegen.codegen prog;

      let modul = Codegen.llvm_module () in
      ExecutionEngine.add_module modul execution_engine;
      let fn = Option.value_exn (Llvm.lookup_function anon_id modul) in
      let result =
        GenericValue.as_int
          (ExecutionEngine.run_function fn [||] execution_engine)
      in
      print_result (Int.to_string result))
