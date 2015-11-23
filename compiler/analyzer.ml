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
  { name = "EUL"; s_type = Num; };
  { name = "PI"; s_type = Num; };
  {
    name = "print";
    s_type = Func({ param_types = [Unconst]; return_type = Unconst; });
  };
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
  | List(l) -> sprintf "list[%s]" (str_of_type l)
  | Func(f) -> str_of_func f
  | Unconst -> "Unconst"

and str_of_func f =
  let param_types = List.map str_of_type f.param_types and
    return_type = str_of_type f.return_type in
  sprintf "func(%s => %s)" (String.concat ", " param_types) return_type

let str_of_unop = function
  | Not -> "!"      | Sub -> "-"

let str_of_binop = function
  | Add -> "+"      | Sub -> "-"
  | Mult -> "*"     | Div -> "/"
  | Mod -> "%"      | Pow -> "**"
  | Eq -> "=="      | Neq -> "!="
  | Less -> "<"     | Leq -> "<="
  | Greater -> ">"  | Geq -> ">="
  (*| And -> "&&"     | Or -> "||"*)

(* Exceptions *)
let var_error name =
  let message = sprintf "Use of variable '%s' is undefined in current scope"
    name in
  failwith message

let unop_error op t = 
  let message = sprintf "Invalid use of unary operator '%s' with type %s"
    (str_of_unop op) (str_of_type t) in
  failwith message

let binop_error t1 op t2 = 
  let message =
    sprintf "Invalid use of binary operator '%s' with type %s and %s" 
    (str_of_binop op) (str_of_type t1) (str_of_type t2) in
  failwith message

let fcall_error id f =
  let name = match id with
    | Sast.Id(name) -> name
    | _ -> failwith "Sast.Call provided non-ID as first argument" in
  let message = sprintf "Invalid call of function '%s' with type %s"
    name (str_of_func f) in
  failwith message

let list_error list_type elem_type = 
  let message = sprintf "Invalid element of type %s in list of type %s"
    (str_of_type elem_type) (str_of_type list_type) in
  failwith message

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
    with Not_found -> var_error id in
  env, Sast.Expr(Sast.Id(var.name), var.s_type)

and check_unop env op e =
  let env', ew = check_expr env e in
  let Sast.Expr(e, typ) = ew in
  match op with
    | Not -> begin match typ with
      | Bool -> env', Sast.Expr(Sast.Unop(op, ew), Bool)
      (* constrain types here *)
      | Unconst -> env', Sast.Expr(Sast.Unop(op, ew), Bool)
      | _ as t -> unop_error op t
    end
    | Sub -> begin match typ with 
      | Num -> env', Sast.Expr(Sast.Unop(op, ew), Num)
      (* constrain types here *)
      | Unconst -> env', Sast.Expr(Sast.Unop(op, ew), Num)
      | _ as t -> unop_error op t
    end

and check_binop env e1 op e2 =
  let env', ew1 = check_expr env e1 in
  let Sast.Expr(e1, typ1) = ew1 in
  let env', ew2 = check_expr env' e2 in
  let Sast.Expr(e2, typ2) = ew2 in
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
        env', Sast.Expr(Sast.Binop(ew1, op, ew2), result_type)
      else binop_error typ1 op typ2
    | Eq | Neq -> 
      let is_valid_equality = function
        | Num | Bool | String -> true
        (* NO CONSTRAINING CAN BE DONE ON OVERLOADED EQUALITY OPERATOR *)
        | _ -> false in 
      if is_valid_equality typ1 && is_valid_equality typ2 then 
        env', Sast.Expr(Sast.Binop(ew1, op, ew2), Bool)
      else binop_error typ1 op typ2
    (*| And | Or ->
      let is_bool = function
        | Bool -> true
        (* constrain types here *)
        | Unconst -> true
        | _ -> false in
      if is_bool typ1 && is_bool typ2 then
        env', Sast.Expr(Sast.Binop(ew1, op, ew2), Bool)
      else binop_error typ1 op typ2*)

and check_func_call env id args =
  let env', ew = check_expr env id in
  let Sast.Expr(id, typ) = ew in
  let f = match typ with
    | Sast.Func(f) -> f
    | _ -> failwith "Attempting to call a non-function" in
  let args = check_func_call_args env' id f args in
  env', Sast.Expr(Sast.Call(ew, args), f.return_type)

and check_func_call_args env id f args =
  if List.length f.param_types <> List.length args then fcall_error id f else
  let rec aux acc param_types = function
    | [] -> List.rev acc
    | (Sast.Expr(e, typ) as ew) :: tl -> let const = List.hd param_types in
      (* TODO: constrain types if possible *)
      if (typ = const || const = Unconst) then
        aux (ew :: acc) (List.tl param_types) tl
      else fcall_error id f in
  (* TODO: don't throw away env from args here *)
  aux [] f.param_types (List.map (fun e -> snd(check_expr env e)) args)

and check_assign env id = function
  | Ast.Fdecl(f) -> check_fdecl env id f
  | _ as ew -> let env', ew' = check_expr env ew in
      let Sast.Expr(_, typ) = ew' in
      let env', name = add_to_scope env' id typ in
      env', Sast.Expr(Sast.Assign(name, ew'), typ)

and check_list env l =
  if List.length l = 0 then env, Sast.Expr(Sast.List([]), List(Unconst)) else
  let rec aux acc const = function
    | [] -> Sast.Expr(Sast.List(List.rev acc), List(const))
    | (Sast.Expr(e, typ) as ew) :: tl ->
      (* TODO: constrain type to const *)
      if typ = const || const = Unconst then
        aux (ew :: acc) const tl
      else list_error (List(const)) typ in
  (* TODO: don't throw away env from args here *)
  let ew_list = List.map (fun e -> snd(check_expr env e)) l in
  let Sast.Expr(_, const) = List.hd ew_list in
  env, aux [] const ew_list

and check_fdecl env id f =
  let env', name = add_to_scope env id Unconst in
  let func_env, param_ids = check_fdecl_params env' f.params in
  let func_env, body = check_stmts func_env f.body in
  let func_env, return = check_expr func_env f.return in
  let Sast.Expr(_, ret_type) = return in
  let fdecl = {
    fname = name;
    params = param_ids;
    body = body;
    return = return;
  } in
  let param_types = 
    List.map (fun name -> (VarMap.find name func_env.scope).s_type) f.params in
  let f_type = { param_types = param_types; return_type = ret_type } in
  env', Sast.Expr(Sast.Fdecl(fdecl), Func(f_type))

and check_fdecl_params env param_list =
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | param :: tl -> let env', name = add_to_scope env param Unconst in
        aux env' (name :: acc) tl
  in aux env [] param_list

and check_stmt env = function
  | Ast.Do(e) -> let env', ew = check_expr env e in env', Sast.Do(ew)

and check_stmts env stmt_list = 
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | stmt :: tl -> let env', e = check_stmt env stmt in
        aux env' (e :: acc) tl
  in aux env [] stmt_list

let check_ast ast = 
  let _, sast = check_stmts root_env ast in sast
