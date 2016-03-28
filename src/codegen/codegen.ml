open Core.Std
open Ollvm.Ez.Value
open Ollvm.Ez.Block
open Ollvm.Ez.Instr
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module Gateway = Ollvm_llvmgateway
open Llvm_target

(* Mutables for JIT state *)
let _target_machine : TargetMachine.t option ref = ref None
let _cg_module : M.t option ref = ref None
let syms : Ollvm.Ez.Value.t String.Table.t = String.Table.create ()
let target_machine () = Option.value_exn !_target_machine
let cg_module      () = Option.value_exn !_cg_module
let llvm_module    () = (Gateway.modul (cg_module ()).m_module).m

let rec codegen_expr (m : M.t) (e : Ast.expr) (ret : Ollvm.Ez.Value.t) =
  match e with
  | Ast.Number n ->
    (m, [ret <-- add (i32 0) (i32 n)])
  | Ast.Variable id ->
    let x = String.Table.find_exn syms id in
    (m, [ret <-- add (i32 0) x])
  | Ast.Binary (op, e1, e2) ->
    let (m, [x1; x2]) = M.locals m T.i32 [""; ""] in
    let (m, ins1) = codegen_expr m e1 x1 in
    let (m, ins2) = codegen_expr m e2 x2 in
    let instr = match op with
      | Ast.Add      -> ret <-- add x1 x2
      | Ast.Subtract -> ret <-- sub x1 x2
      | Ast.Multiply -> ret <-- mul x1 x2
      | Ast.Divide   -> ret <-- sdiv x1 x2
    in
    (m, ins1 @ ins2 @ [instr])
  | Ast.Call (id, args) ->
    let (m, temps) = M.locals m T.i32 (List.map ~f:(fun _ -> "") args) in
    let foldr (m, instrs) e tmp =
      let (m, new_instrs) = codegen_expr m e tmp in
      (m, instrs @ new_instrs)
    in
    let (m, instrs) = List.fold2_exn args temps ~init:(m, []) ~f:foldr in
    let (m, fid) = M.global m T.i32 id in
    (m, instrs @ [ret <-- call fid temps])

(* Create an LLVM identifier for each argument to a function *)
let rec bind_args (m : M.t) (args : Ast.ident list) =
  List.fold args ~init:(m, []) ~f:(fun (m, bound) arg ->
    let (m, x) = M.local m T.i32 arg in
    let () = String.Table.add_exn syms ~key:arg ~data:x in
    (m, x :: bound))

let codegen_func (m : M.t) (Ast.Function (proto, expr)) =
  let Ast.Prototype (fname, args) = proto in
  let (m, retvar) = M.local m T.i32 "" in
  let (m, fargs)  = bind_args m args in
  let (m, fexpr)  = codegen_expr m expr retvar in
  let (m, fblock) = M.local m T.label fname in
  let (m, fid)    = M.global m T.i32 fname in
  let f = define fid fargs [block fblock (fexpr @ [ret retvar]) ] in
  M.definition m f

let codegen_proto (Ast.Prototype (fname, args)) =
  let m = cg_module () in
  let (m, retvar) = M.local m T.i32 "" in
  let (m, fargs)  = bind_args m args in
  let (m, fid)    = M.global m T.i32 fname in
  let m = M.definition m (define fid fargs []) in
  _cg_module := Some m

let codegen (Ast.Program functions) =
  let m = List.fold_left functions ~init:(cg_module ()) ~f:codegen_func in
  _cg_module := Some m

let emit_object file =
  TargetMachine.emit_to_file
    (llvm_module ()) CodeGenFileType.ObjectFile file (target_machine ())

let init () =
  let () = Llvm_all_backends.initialize () in
  let target = Target.by_triple (Target.default_triple()) in
  _target_machine :=
    Some (TargetMachine.create (Target.default_triple ()) target);
  let arch :: platform :: os =
    Str.split (Str.regexp "-") (TargetMachine.triple (target_machine ()))
  in
  _cg_module :=
    Some
      (M.init
         "y0_llvm_module"
         (arch, platform,
          Str.string_after (List.fold os ~init:"" ~f:(fun a b -> a ^ "-" ^ b)) 1)
         (DataLayout.as_string (TargetMachine.data_layout (target_machine ()))))
