open Core.Std

let () =
  let open Command.Let_syntax in
  Command.basic'
    ~summary:"y0 compiler"
    [%map_open
      let run_jit = flag "-jit" no_arg ~doc:"Run JIT interpreter"
      and verbose = flag "-v" no_arg ~doc: "Verbose mode"
      and path = anon (maybe ("path" %: string))
      in fun () ->
        Codegen.init();
        if run_jit then
          Jit.start verbose
        else
          Compiler.compile
            (Option.value_exn ~message:"Path is missing" path)
            verbose
    ]
  |> Command.run
