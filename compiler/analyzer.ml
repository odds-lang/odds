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

let gen_check_expr = function
  | Ast.Int_lit(i) -> Sast.Int_lit(i)
  | Ast.String_lit(s) -> Sast.String_lit(s)
  | Ast.Bool_lit(b) -> env, String.capitalize (string_of_bool(b))
  | Ast.Id(id) -> env, txt_of_id env id
  | Ast.Unop(op, e) -> let _, e = txt_of_expr env e in 
      env, sprintf "(%s%s)" (txt_of_unop op) e
  | Ast.Binop(e1, op, e2) ->
      let _, e1 = txt_of_expr env e1 and _, e2 = txt_of_expr env e2 in
      env, sprintf "(%s %s %s)" e1 (txt_of_binop op) e2
  | Ast.Call(f, args) -> let _, id = txt_of_expr env f in
      env, sprintf "%s(%s)" id (txt_of_list env args)
  | Ast.Assign(id, e) -> txt_of_assign env id e
  | Ast.List(l) -> let e = txt_of_list env l in env, sprintf "[%s]" e
  | Ast.Fdecl(f) -> txt_of_fdecl env "anon" f

let gen_check_stmt = function
  | Ast.Do(e) -> Sast.stmt(gen_check_expr e)

let gen_check_program program =
  List.fold_left gen_check_stmt [] program
