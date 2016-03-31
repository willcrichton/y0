open Core.Std

let compile path verbose =
  let lexbuf = Lexing.from_channel (open_in path) in
  let ast = Util.parse_buf My_parser.prog lexbuf in
  Codegen.codegen ast;

  (* Write out the object file and compile it to a binary *)
  let obj_file = Filename.temp_file "" "" in
  Codegen.emit_object obj_file;
  let output_path = (Filename.chop_extension (Filename.basename path)) in
  let err = Sys.command (sprintf "gcc %s -o %s" obj_file output_path) in
  if err <> 0 then failwith "Compilation failed"
  else ()
