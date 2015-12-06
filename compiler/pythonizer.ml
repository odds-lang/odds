(*
 * COMS4115: Python AST Generator
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Sast
open Past

exception Python_Error of string

(* Expressions *)
let rec past_expr stmts = function
  | Sast.Num_lit(n) -> stmts, Past.Num_lit(n)
  | Sast.String_lit(s) -> stmts, Past.String_lit(s)
  | Sast.Bool_lit(b) -> stmts, Past.Bool_lit(b)
  | Sast.Void_lit -> stmts, Past.None_lit
  | Sast.Id(id) -> stmts, Past.Id(id)
  | Sast.Unop(op, we) -> let stmts', e = past_expr_unwrap stmts we in
      stmts', Past.Unop(op, e)
  | Sast.Binop(we1, op, we2) ->
      let stmts', e1 = past_expr_unwrap stmts we1 in
      let stmts', e2 = past_expr_unwrap stmts we2 in
      stmts', Past.Binop(e1, op, e2)
  | Sast.Call(wid, wargs) ->
      let stmts', id = past_expr_unwrap stmts wid in
      let stmts', args = past_list stmts wargs in
      stmts', Past.Call(id, args)
  | Sast.List(wl) -> let stmts', l = past_list stmts wl in stmts', Past.List(l)
  | _ -> raise (Python_Error "Statement Within Expression")

and mk_stmt stmts = function 
  | Sast.Assign(id, we) -> let stmts', e = past_expr_unwrap stmts we in
      stmts', Past.Assign(id, e)
  | Sast.Fdecl(f) -> if f.is_anon then past_fdecl_anon stmts f
      else past_fdecl stmts f
  | Sast.If(cond) -> mk_if_function stmts cond
  | _ as e -> let stmt', e' = past_expr stmts e in
      stmt', Past.Stmt(e')
 
and past_expr_unwrap stmts = function
  | Sast.Expr(e, _) -> past_expr stmts e

and past_stmt_unwrap stmts = function 
  | Sast.Expr(e, _) -> mk_stmt stmts e

(* Lists *)
and past_list stmts expr_list =
  let rec aux stmts acc = function
    | [] -> stmts, List.rev acc
    | we :: tl -> let stmts', e = past_expr_unwrap stmts we in
        aux stmts' (e :: acc) tl
  in aux stmts [] expr_list

(* Functions *)
and mk_if_function stmts cond = 
    let stmts', e1' = past_expr_unwrap stmts cond.cond in
    let stmts', e2' = past_expr_unwrap stmts cond.stmt_1 in
    let stmts', e3' = past_expr_unwrap stmts cond.stmt_2 in
    let r1 = Past.Return(e2') in
    let r2 = Past.Return(e3') in
    let if_stmt = Past.If(e1', r1, r2) in 
    let func = {
      p_name = cond.cond_name;
      p_params = [];
      p_body = [if_stmt];
    } in
    let def = Def(func) in
    let stmts' = (def:: stmts') in 
    let call = Past.Call(Past.Id(func.p_name), []) in
    match cond.is_assigned with
      | true -> stmts', Past.Assign(cond.id_assign, call)
      | false -> stmts', Past.Stmt(call)

and past_fdecl_anon stmts sast_f =
  let stmts', def = past_fdecl stmts sast_f in
  let stmts' = ( def :: stmts') in
  match def with
    | Past.Def(f) -> stmts', Past.Stmt (Past.Id(f.p_name))
    | _ -> failwith "past_fdecl() returned non Past.Def type"

and past_fdecl stmts sast_f =
  let b = past_stmts sast_f.body in
  let stmts', e = past_expr_unwrap stmts sast_f.return in
  let r = Past.Return(e) in 
  let body = b @ [r] in
  let f = {
    p_name = sast_f.fname;
    p_params = sast_f.params;
    p_body = body;
  } in stmts', Past.Def(f)

(* Statements *)
and past_stmt stmts = function
  | Sast.Do(we) -> let stmts', e = past_stmt_unwrap stmts we in
      stmts', e

and past_stmts stmt_list = 
  let rec aux acc = function
    | [] -> List.rev acc
    | stmt :: tl -> let stmts', s = past_stmt acc stmt in
        aux (s :: stmts') tl
  in aux [] stmt_list

(* Program entry point *)
let generate_past sast = past_stmts sast
