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
      let stmts', e2 = past_expr_unwrap stmts' we2 in
      stmts', Past.Binop(e1, op, e2)
  | Sast.Call(wid, wargs) ->
      let stmts', id = past_expr_unwrap stmts wid in
      let stmts', args = past_list stmts' wargs in
      stmts', Past.Call(id, args)
  | Sast.Ldecl(wl) ->
      let stmts', l = past_list stmts wl in stmts', Past.Ldecl(l)
  | Sast.Dist(d) -> past_dist stmts d
  | Sast.Discr_dist(d) -> past_discr_dist stmts d
  | Sast.Assign(id, we) -> let stmts', e = past_expr_unwrap stmts we in
      (Past.Assign(id, e) :: stmts'), Past.Id(id)
  | Sast.Fdecl(f) -> let stmts', def = past_fdecl stmts f in
      (Past.Def(def) :: stmts'), Past.Id(def.p_name)
  | Sast.Cake(wfdecl, wcall) -> let stmts', _ = past_expr_unwrap stmts wfdecl in
      past_expr_unwrap stmts' wcall
  | Sast.If(cond) -> mk_if_function stmts cond

and past_expr_unwrap stmts = function
  | Sast.Expr(e, _) -> past_expr stmts e

(* Distributions *)
and past_dist stmts d =
  let stmts1, min' = past_expr_unwrap stmts d.min in
  let stmts2, max' = past_expr_unwrap stmts1 d.max in
  let stmts3, dist_func' = past_expr_unwrap stmts2 d.dist_func in
  stmts3, Past.Call(Past.Id("make_dist"), [min' ; max' ; dist_func'])

and past_discr_dist stmts d = 
  let stmts1, vals' = past_expr_unwrap stmts d.vals in
  let stmts2, weights' = past_expr_unwrap stmts1 d.weights in
  stmts2, Past.Call(Past.Id("make_discr_dist"), [vals' ; weights'])



(* Lists *)
and past_list stmts expr_list =
  let rec aux stmts acc = function
    | [] -> stmts, List.rev acc
    | we :: tl -> let stmts', e = past_expr_unwrap stmts we in
        aux stmts' (e :: acc) tl
  in aux stmts [] expr_list

(* Functions *)
and mk_if_function stmts cond = 
    let stmts', i = past_expr_unwrap stmts cond.cond in
    let stmts', t = past_expr_unwrap stmts' cond.stmt_1 in
    let stmts', e = past_expr_unwrap stmts' cond.stmt_2 in
    let r1 = Past.Return(t) in
    let r2 = Past.Return(e) in
    let if_stmt = Past.If(i, r1, r2) in 
    let f = {
      p_name = cond.c_name;
      p_params = [];
      p_body = [if_stmt];
    } in
    let stmts' = (Def(f) :: stmts') in
    stmts', Past.Call(Past.Id(f.p_name), [])

and past_fdecl stmts sast_f =
  let body = past_stmts sast_f.body in
  let body', e = past_expr_unwrap body sast_f.return in
  let r = Past.Return(e) in 
  let body' = body' @ [r] in
  let f = {
    p_name = sast_f.f_name;
    p_params = sast_f.params;
    p_body = body';
  } in stmts, f

(* Statements *)
and past_stmt stmts = function
  | Sast.Do(we) -> let stmts', e = past_expr_unwrap stmts we in stmts', e

and past_stmts stmt_list = 
  let rec aux acc = function
    | [] -> List.rev acc
    | stmt :: tl -> let stmts', s = past_stmt acc stmt in
        aux (Past.Stmt(s) :: stmts') tl
  in aux [] stmt_list

(* Program entry point *)
let generate_past sast = past_stmts sast
