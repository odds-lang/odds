(*
 * COMS4115: Python AST Generator
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Ast
open Sast
open Past

let rec past_expr stmts = function
  | Sast.Num_lit(n) -> stmts, Num_lit(n)
  | Sast.String_lit(s) -> stmts, String_lit(s)
  | Sast.Bool_lit(b) -> stmts, Bool_lit(b)
  | Sast.Void_lit -> stmts, Void_lit
  | Sast.Id(id) -> stmts, Id(id)
  | Sast.Unop(op, we) -> let stmts', e = past_expr_unwrap stmts we in
      stmts', Unop(op, e)
  | Sast.Binop(we1, op, we2) ->
      let stmts', e1 = past_expr_unwrap stmts we1 in
      let stmts', e2 = past_expr_unwrap stmts we2 in
      stmts', Binop(e1, op, e2)
  | Sast.Call(wid, wargs) ->
      let stmts', id = past_expr_unwrap stmts wid in
      let stmts', args = past_list stmts wargs in
      stmts', Call(id, args)
  | Sast.Assign(id, we) -> let stmts', e = past_expr_unwrap stmts we in
      stmts', Assign(id, e)
  | Sast.List(wl) -> let stmts', l = past_list stmts wl in stmts', List(l)
  | Sast.Fdecl(f) -> if f.is_anon = true then past_fdecl_anon stmts f
      else stmts, past_fdecl stmts f

and past_expr_unwrap stmts = function
  | Sast.Expr(e, _) -> past_expr stmts e

and past_list stmts expr_list =
  let rec aux stmts acc = function
    | [] -> stmts, List.rev acc
    | we :: tl -> let stmts', e = past_expr_unwrap stmts we in
        aux stmts' (e :: acc) tl
  in aux stmts [] expr_list

and past_fdecl_anon stmts sast_f =
  let f = past_fdecl stmts sast_f in let stmts' = (f :: stmts) in
    stmts', Id(f.name)

and past_fdecl stmts sast_f =
  let b = past_stmts sast_f.body in
  let e = past_expr_unwrap stmts sast_f.return in
  let f = {
    fname = sast_f.name;
    params = sast_f.params;
    body = b;
    return = e;
  } in Def(f)

and past_stmt stmts stmt = function
  | Sast.Do(we) -> past_expr_unwrap stmts we

and past_stmts stmt_list = 
  let rec aux acc = function
    | [] -> List.rev acc
    | stmt :: tl -> let stmts, e = past_stmt acc stmt in
        aux (e :: stmts) tl
  in aux [] stmt_list

(* Program entry point *)
let generate_past sast = 
  past = past_stmts sast