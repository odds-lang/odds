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
open Printf

module StringMap = Map.Make(String)

(* Environment *)
type environment = {
  reserved: string list;
  scope: string StringMap.t;
}

let root_env = {
  reserved = ["print"];
  scope = StringMap.empty;
}

(* Exceptions *)
exception Error of string

(* Static scoping variable counter *)
let ss_counter = ref (-1)
let get_ss_id name =
  ss_counter := !ss_counter + 1;
  sprintf "%s_%d" name !ss_counter

let add_to_scope env id =
  let ss_id = get_ss_id id in
  let new_env = {
    reserved = env.reserved;
    scope = StringMap.add id ss_id env.scope;
  } in
  new_env, ss_id

(* Checks *)
let rec check_expr env = function
  | Ast.Num_lit(x) -> env, Sast.Num_lit(x)
  | Ast.String_lit(s) -> env, Sast.String_lit(s)
  | Ast.Bool_lit(b) -> env, Sast.Bool_lit(b)
  | Ast.Id(id) -> check_id env id
  | Ast.Unop(op, e) -> check_unop env op e
  | Ast.Binop(e1, op, e2) -> check_binop env e1 op e2
  | Ast.Call(f, args) -> check_func_call env f args
  | Ast.Assign(id, e) -> check_assign env id e
  | Ast.List(l) -> check_list env l
  | Ast.Fdecl(f) -> check_fdecl env "anon" f

and check_id env id =
  if List.mem id env.reserved then env, Sast.Id(id) else
  if StringMap.mem id env.scope then env, Sast.Id(StringMap.find id env.scope)
  else let error = sprintf "ID '%s' not found." id in raise (Error(error))

and check_unop env op e =
  let _, e = check_expr env e in env, Sast.Unop(op, e)

and check_binop env e1 op e2 =
  let _, e1 = check_expr env e1 and _, e2 = check_expr env e2 in
  env, Sast.Binop(e1, op, e2)

and check_func_call env f args =
  let _, id = check_expr env f in
  let args = List.map (fun e -> snd(check_expr env e)) args in
  env, Sast.Call(id, args)

and check_assign env id = function
  | Ast.Fdecl(f) -> check_fdecl env id f
  | _ as e -> let new_env, e = check_expr env e in
      let new_env, name = add_to_scope new_env id in
      new_env, Sast.Assign(name, e)

and check_list env l =
  let l = List.map (fun e -> snd(check_expr env e)) l in env, Sast.List(l)

and check_fdecl env id f = 
  let new_env, name = add_to_scope env id in
  let func_env, params = check_fdecl_params new_env f.params in
  let func_env, body = check_stmts func_env f.body in
  let _, return = check_expr func_env f.return in
  let fdecl = {
    name = name;
    params = params;
    body = body;
    return = return;
  }
  in new_env, Sast.Fdecl(fdecl)

and check_fdecl_params env param_list =
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | Ast.Id(param) :: tl -> let new_env, name = add_to_scope env param in
        aux new_env (Sast.Id(name) :: acc) tl
    | _ -> raise (Error("Invalid function parameter."))
  in aux env [] param_list

and check_stmt env = function
  | Ast.Do(e) -> let new_env, e = check_expr env e in new_env, Sast.Do(e)

and check_stmts env stmt_list = 
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | stmt :: tl -> let new_env, e = check_stmt env stmt in
        aux new_env (e :: acc) tl
  in aux env [] stmt_list

let check_program program = snd(check_stmts root_env program)
