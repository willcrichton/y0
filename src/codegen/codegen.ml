open Ollvm.Ez.Value
open Ollvm.Ez.Block
open Ollvm.Ez.Instr
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
open Llvm_target

let module_name = "y0"
let symtable_initial_size = 8

let rec codegen_expr m syms e ret =
  match e with
  | Ast.Number n ->
    (m, [ret <-- add (i32 0) (i32 n)])
  | Ast.Variable id ->
    let x = Hashtbl.find syms id in
    (m, [ret <-- add (i32 0) x])
  | Ast.Binary (op, e1, e2) ->
    let (m, [x1; x2]) = M.locals m T.i32 [""; ""] in
    let (m, ins1) = codegen_expr m syms e1 x1 in
    let (m, ins2) = codegen_expr m syms e2 x2 in
    let instr = match op with
      | Ast.Add -> ret <-- add x1 x2
      | Ast.Subtract -> ret <-- sub x1 x2
      | Ast.Multiply -> ret <-- mul x1 x2
      | Ast.Divide -> ret <-- sdiv x1 x2
    in
    (m, ins1 @ ins2 @ [instr])
  | Ast.Call (id, args) ->
    let (m, temps) = M.locals m T.i32 (List.map (fun _ -> "") args) in
    let foldr (m, instrs) e tmp =
      let (m, new_instrs) = codegen_expr m syms e tmp in
      (m, instrs @ new_instrs)
    in
    let (m, instrs) = List.fold_left2 foldr (m, []) args temps in
    let (m, fid) = M.global m T.i32 id in
    (m, instrs @ [ret <-- call fid temps])

let rec bind_args m syms args =
  match args with
  | [] -> (m, [])
  | arg :: rest ->
    let (m, x) = M.local m T.i32 arg in
    let _ = Hashtbl.add syms arg x in
    let (m, bound) = bind_args m syms rest in
    (m, x :: bound)

let codegen_func syms m (Ast.Function (proto, expr)) =
  let Ast.Prototype (fname, args) = proto in
  let (m, retvar) = M.local m T.i32 "" in
  let (m, fargs)  = bind_args m syms args in
  let (m, fexpr)  = codegen_expr m syms expr retvar in
  let (m, fblock) = M.local m T.label fname in
  let (m, fid)    = M.global m T.i32 fname in
  let f = define fid fargs [block fblock (fexpr @ [ret retvar]) ] in
  M.definition m f

let codegen (Ast.Program functions) target_machine =
  let arch :: platform :: os =
    Str.split (Str.regexp "-") (TargetMachine.triple target_machine) in
  let m =
    M.init
      module_name
      (arch, platform,
       Str.string_after (List.fold_left (fun a b -> a ^ "-" ^ b) "" os) 1)
      (DataLayout.as_string (TargetMachine.data_layout target_machine))
  in
  let syms = Hashtbl.create symtable_initial_size in
  let m = List.fold_left (codegen_func syms) m functions in
  (Ollvm_llvmgateway.modul m.m_module).m
