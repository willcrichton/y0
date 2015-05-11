open Ollvm.Ez.Value
open Ollvm.Ez.Block
open Ollvm.Ez.Instr
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module P = Ollvm.Printer

module L = Llvm

let rec codegen_expr m syms e ret =
  match e with
  | Ast.Number(n) ->
     (m, [ret <-- add (i32 0) (i32 n)])
  | Ast.Variable(id) ->
     let x = Hashtbl.find syms id in
     (m, [ret <-- add (i32 0) x])
  | Ast.Binary(op, e1, e2) ->
     let (m, [x1; x2]) = M.locals m T.i32 [""; ""] in
     let (m, ins1) = codegen_expr m syms e1 x1 in
     let (m, ins2) = codegen_expr m syms e2 x2 in
     (m, ins1 @ ins2 @ [ret <-- add x1 x2])
  | _ -> exit(-1)

let rec bind_args m syms args =
  match args with
  | [] -> (m, [])
  | arg :: rest ->
     let (m, x) = M.local m T.i32 arg in
     let _ = Hashtbl.add syms arg x in
     let (m, bound) = bind_args m syms rest in
     (m, x :: bound)

let codegen_func syms m (Ast.Function(proto, expr)) =
  let Ast.Prototype(fname, args) = proto in
  let (m, retvar) = M.local m T.i32 "" in
  let (m, fargs)  = bind_args m syms args in
  let (m, fexpr)  = codegen_expr m syms expr retvar in
  let (m, fblock) = M.local m T.label fname in
  let (m, fid)    = M.global m T.i32 fname in
  let f = define fid fargs [block fblock (fexpr @ [ret retvar]) ] in
  let m = M.definition m f in m

let codegen (Ast.Program(functions)) =
  let m = M.init
            "name"
            ("x86_64", "apple", "macosx10.10.0")
            "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128" in
  let syms = Hashtbl.create 8 in
  let m = List.fold_left (codegen_func syms) m functions in
  () (*Ollvm_llvmgateway.modul m.m_module*)

  (*let tmp_path = Filename.temp_file "" "" in
  let tmp_file = open_out tmp_path in
  let _ = output_value tmp_file slow in
  read_process ("llvm-as < " ^ tmp_path ^ " | opt -S -mem2reg -O2")*)
