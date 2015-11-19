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
  scope: Sast.var StringMap.t;
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

let add_to_scope env id s_type =
  let ss_id = get_ss_id id in
  let var = { name = ss_id; s_type = s_type } in
  let env' = {
    reserved = env.reserved;
    scope = StringMap.add id var env.scope;
  } in
  env', ss_id

(* Utilities *)
let str_of_type = function
  | Num -> "num"
  | String -> "string"
  | Bool -> "bool"
  | List -> "list"
  | Unconst -> "Unconst"

(* Checks *)
let rec check_expr env = function
  | Ast.Num_lit(x) -> env, Sast.Expr(Sast.Num_lit(x), Num)
  | Ast.String_lit(s) -> env, Sast.Expr(Sast.String_lit(s), String)
  | Ast.Bool_lit(b) -> env, Sast.Expr(Sast.Bool_lit(b), Bool)
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
  let raise_error op t = 
    let message = sprintf "Invalid use of '%s' with %s" in
    raise (Error(message)) in
  let _, Sast.Expr(e, typ) = check_expr env e in
  match op with
    | Not -> begin match typ with
      | Bool -> env, Sast.Expr(Sast.Unop(op, e), Bool)
      (* constrain types here *)
      | Unconst -> env, Sast.Expr(Sast.Unop(op, e), Bool)
      | _ as t -> raise_error "!" (str_of_type t)
    end
    | Minus -> begin match typ with 
      | Num -> env, Sast.Expr(Sast.Unop(op, e), Num)
      (* constrain types here *)
      | Unconst -> env, Sast.Expr(Sast.Unop(op, e), Num)
      | _ as t -> raise_error "-" (str_of_type t)
    end

and check_binop env e1 op e2 =
  let raise_error t = 
  let _, e1 = check_expr env e1 and _, e2 = check_expr env e2 in
  env, Sast.Binop(e1, op, e2)

and check_func_call env f args =
  let _, id = check_expr env f in
  let args = List.map (fun e -> snd(check_expr env e)) args in
  env, Sast.Call(id, args)

and check_assign env id = function
  | Ast.Fdecl(f) -> check_fdecl env id f
  | _ as e -> let env', e = check_expr env e in
      let env', name = add_to_scope env' id Unconst in
      env', Sast.Assign(name, e)

and check_list env l =
  let l = List.map (fun e -> snd(check_expr env e)) l in env, Sast.List(l)

and check_fdecl env id f = 
  let env', name = add_to_scope env id Unconst in
  let func_env, params = check_fdecl_params env' f.params in
  let func_env, body = check_stmts func_env f.body in
  let _, return = check_expr func_env f.return in
  let fdecl = {
    name = name;
    params = params;
    body = body;
    return = return;
  }
  in env', Sast.Fdecl(fdecl)

and check_fdecl_params env param_list =
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | Ast.Id(param) :: tl -> let env', name = add_to_scope env param Unconst in
        aux env' (Sast.Id(name) :: acc) tl
    | _ -> raise (Error("Invalid function parameter."))
  in aux env [] param_list

and check_stmt env = function
  | Ast.Do(e) -> let env', e = check_expr env e in env', Sast.Do(e)

and check_stmts env stmt_list = 
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | stmt :: tl -> let env', e = check_stmt env stmt in
        aux env' (e :: acc) tl
  in aux env [] stmt_list

let check_ast ast = 
  let _, sast = check_stmts root_env ast in sast
