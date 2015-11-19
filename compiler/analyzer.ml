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

module VarMap = Map.Make(String)

(* Environment *)
type environment = {
  reserved: string list;
  scope: Sast.var VarMap.t;
}

let root_env = {
  reserved = ["print"];
  scope = VarMap.empty;
}

(* Utilities *)
let str_of_type = function
  | Num -> "num"          | String -> "string"
  | Bool -> "bool"        | List -> "list"
  | Unconst -> "Unconst"

let str_of_unop = function
  | Not -> "!"
  | Sub -> "-"

let str_of_binop = function
  | Add -> "+"      | Sub -> "-"
  | Mult -> "*"     | Div -> "/"
  | Mod -> "%"      | Pow -> "**"
  | Eq -> "=="      | Neq -> "!="
  | Less -> "<"     | Leq -> "<="
  | Greater -> ">"  | Geq -> ">="
  | And -> "&&"     | Or -> "||"

(* Exceptions *)
exception Error of string

let var_error id =
  let message = sprintf "Use of variable '%s' is undefined in current scope" id
  in raise (Error(message))

let unop_error op t = 
  let message = sprintf "Invalid use of unary operator '%s' with type %s"
    (str_of_unop op) (str_of_type t) in
  raise (Error(message))

let binop_error t1 op t2 = 
  let message = sprintf "Invalid use of binary operator '%s' with type %s and %s" 
    (str_of_binop op) (str_of_type t1) (str_of_type t2) in
  raise (Error(message))

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
    scope = VarMap.add id var env.scope;
  } in
  env', ss_id

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
  if List.mem id env.reserved then env, Sast.Expr(Sast.Id(id), Unconst) else
  if VarMap.mem id env.scope then
    let var = VarMap.find id env.scope in
    env, Sast.Expr(Sast.Id(var.name), var.s_type)
  else var_error id

and check_unop env op e =
  let _, Sast.Expr(e, typ) = check_expr env e in
  match op with
    | Not -> begin match typ with
      | Bool -> env, Sast.Expr(Sast.Unop(op, e), Bool)
      (* constrain types here *)
      | Unconst -> env, Sast.Expr(Sast.Unop(op, e), Bool)
      | _ as t -> unop_error op t
    end
    | Sub -> begin match typ with 
      | Num -> env, Sast.Expr(Sast.Unop(op, e), Num)
      (* constrain types here *)
      | Unconst -> env, Sast.Expr(Sast.Unop(op, e), Num)
      | _ as t -> unop_error op t
    end

and check_binop env e1 op e2 =
  let _, Sast.Expr(e1, typ1) = check_expr env e1 
  and _, Sast.Expr(e2, typ2) = check_expr env e2 in
  match op with
    | Add | Sub | Mult | Div | Mod | Pow | Less | Leq | Greater | Geq -> 
      let is_num = function
        | Num -> true
        (* constrain types here *)
        | Unconst -> true
        | _ -> false in 
      if is_num typ1 && is_num typ2 then 
        let result_type = match op with
          | Add | Sub | Mult | Div | Mod | Pow -> Num
          | Less | Leq | Greater | Geq -> Bool
          | _ -> binop_error typ1 op typ2 in
        env, Sast.Expr(Sast.Binop(e1, op, e2), result_type)
      else binop_error typ1 op typ2
    | Eq | Neq -> 
      let is_valid_equality = function
        | Num | Bool | String -> true
        (* NO CONSTRAINING CAN BE DONE ON OVERLOADED EQUALITY OPERATOR *)
        | _ -> false in 
      if is_valid_equality typ1 && is_valid_equality typ2 then 
        env, Sast.Expr(Sast.Binop(e1, op, e2), Bool)
      else binop_error typ1 op typ2
    | And | Or ->
      let is_bool = function
        | Bool -> true
        (* constrain types here *)
        | Unconst -> true
        | _ -> false in
      if is_bool typ1 && is_bool typ2 then 
        env, Sast.Expr(Sast.Binop(e1, op, e2), Bool)
      else binop_error typ1 op typ2

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
