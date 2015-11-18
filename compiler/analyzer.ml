(*
 * COMS4115: Semantic Analyzer
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Ast
open Sast

let rec check_expr = function
  | Ast.Num_lit(x) -> Sast.Num_lit(x)
  | Ast.String_lit(s) -> Sast.String_lit(s)
  | Ast.Bool_lit(b) -> Sast.Bool_lit(b)
  | Ast.Id(id) -> check_id id
  | Ast.Unop(op, e) -> check_unop op e
  | Ast.Binop(e1, op, e2) -> check_binop e1 op e2
  | Ast.Call(f, args) -> check_func_call f args
  | Ast.Assign(id, e) -> check_assign id e
  | Ast.List(l) -> check_list l
  | Ast.Fdecl(f) -> check_fdecl f

and check_id id =
  Sast.Id(id)

and check_unop op e =
  let e = check_expr e in Sast.Unop(op, e)

and check_binop e1 op e2 =
  let e1 = check_expr e1 and e2 = check_expr e2 in Sast.Binop(e1, op, e2)

and check_func_call f args =
  let id = check_expr f and args = List.map check_expr args in
  Sast.Call(id, args)

and check_assign id e =
  let e = check_expr e in Sast.Assign(id, e)

and check_list l =
  let l = List.map check_expr l in Sast.List(l)

and check_fdecl f = 
  let params = List.map check_expr f.params in
  let body = check_stmts f.body in
  let return = check_expr f.return in
  let fdecl = {
    params = params;
    body = body;
    return = return
  }
  in Sast.Fdecl(fdecl)

and check_stmt = function
  | Ast.Do(e) -> let e = check_expr e in Sast.Do(e)

and check_stmts stmts = 
  List.map check_stmt stmts

let check_program program = check_stmts program
