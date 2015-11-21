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
  reserved: Sast.var list;
  scope: Sast.var VarMap.t;
}

let builtins = [
  { name = "EUL"; s_type = Num; } ;
  { name = "PI"; s_type = Num; };
  { name = "print"; s_type = Func({ param_types = []; return_type = Unconst; })}
]

let root_env = {
  reserved = builtins;
  scope = VarMap.empty;
}

(* Utilities *)
let rec str_of_type = function
  | Num -> "num"
  | String -> "string"
  | Bool -> "bool"
  | List(l) -> sprintf "[%s]" (str_of_type l)
  | Func(f) -> str_of_func f
  | Unconst -> "Unconst"

and str_of_func f =
  let param_types = List.map str_of_type f.param_types and
    return_type = str_of_type f.return_type in
  sprintf "func(%s) -> %s" (String.concat ", " param_types) return_type

let str_of_unop = function
  | Not -> "!"      | Sub -> "-"

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

let var_error name =
  let message = sprintf "Use of variable '%s' is undefined in current scope"
    name
  in raise (Error(message))

let unop_error op t = 
  let message = sprintf "Invalid use of unary operator '%s' with type %s"
    (str_of_unop op) (str_of_type t) in
  raise (Error(message))

let binop_error t1 op t2 = 
  let message =
    sprintf "Invalid use of binary operator '%s' with type %s and %s" 
    (str_of_binop op) (str_of_type t1) (str_of_type t2) in
  raise (Error(message))

let fdecl_param_error () = 
  let message = "Invalid parameter in function declaration" in 
  raise (Error(message))

let fcall_error id f =
  let name = match id with
    | Sast.Id(name) -> name
    | _ -> raise (Error("Sast.Call provided non-ID as first argument")) in
  let message = sprintf "Invalid call of function '%s' with type %s"
    name (str_of_func f) in
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

(* Type Inference/Type Constraints *)
(* This function looks to see if any of the constraints applied in the
 * child should be applied to any of the variables in the parent env *)
let add_constraints child_env parent_env =  
  let c_scope = child_env.scope and p_scope = parent_env.scope in
  let add_constraint raw_id p_var = 
    if p_var.s_type = Sast.Unconst then
      let c_var = VarMap.find raw_id c_scope in 
      if (p_var.name = c_var.name) && (c_var.s_type != Sast.Unconst) then
       let _ =  p_var.s_type <- c_var.s_type in true 
      else false
    else false in
    (* returns true if constraint added, otherwise false. 
     * For debugging purposes only. *)
  let debugger raw_id p_var = (* This function for debugging purposes only *)
    if add_constraint raw_id p_var then 
      let message = sprintf 
          "constrained var with raw id: %s, ss_id: %s, type: %s\n"
          raw_id p_var.name (str_of_type p_var.s_type) in
      print_string message in
  VarMap.iter debugger p_scope; { parent_env with scope = p_scope }


(* Checks *)
let rec check_expr env = function
  | Ast.Num_lit(x) -> env, Sast.Expr(Sast.Num_lit(x), Num)
  | Ast.String_lit(s) -> env, Sast.Expr(Sast.String_lit(s), String)
  | Ast.Bool_lit(b) -> env, Sast.Expr(Sast.Bool_lit(b), Bool)
  | Ast.Id(id) -> check_id env id
  | Ast.Unop(op, e) -> check_unop env op e
  | Ast.Binop(e1, op, e2) -> check_binop env e1 op e2
  | Ast.Call(id, args) -> check_func_call env id args
  | Ast.Assign(id, e) -> check_assign env id e
  | Ast.List(l) -> check_list env l
  | Ast.Fdecl(f) -> check_fdecl env "anon" f

and check_id env id =
  let var =
    if VarMap.mem id env.scope then VarMap.find id env.scope else
    try List.find (fun var -> id = var.name) env.reserved
    with Not_found -> var_error id
  in env, Sast.Expr(Sast.Id(var.name), var.s_type)

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

and check_func_call env id args =
  let _, Sast.Expr(id, typ) = check_expr env id in
  let f = match typ with
    | Sast.Func(f) -> f
    | _ -> raise (Error("Attempting to call a non-function")) in
  let args = check_func_call_args env id f args in
  env, Sast.Expr(Sast.Call(id, args), f.return_type)

and check_func_call_args env id f args =
  if List.length f.param_types <> List.length args then fcall_error id f else
  let rec aux acc param_types = function
    | [] -> List.rev acc
    | Sast.Expr(e, typ) :: tl -> let param_const = List.hd param_types in
      (* TODO: allow unconstrained types, constrain if possible *)
      if typ = param_const then aux (e :: acc) (List.tl param_types) tl
      else fcall_error id f
  in aux [] f.param_types (List.map (fun e -> snd(check_expr env e)) args)

and check_assign env id = function
  | Ast.Fdecl(f) -> check_fdecl env id f
  | _ as e -> let env', e = check_expr env e in
      let env', name = add_to_scope env' id Unconst in
      env', Sast.Assign(name, e)

and check_list env l =
  (* TODO: enforce that they're all the same type, then return
     Sast.Expr(Sast.List(l), List(type)) *)
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
  } in
  let param_type_list = List.map (fun var -> var.s_type) params in
  let func_type = { param_types = param_type_list; return_type = return.s_type } in
  env', Sast.Expr(Sast.Fdecl(fdecl), Sast.Func(func_type))

and check_fdecl_params env param_list =
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | Ast.Id(param) :: tl -> let env', name = add_to_scope env param Unconst in
        aux env' (Sast.Id(name) :: acc) tl
    | _ -> f_decl_param_error ()
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
