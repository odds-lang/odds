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


(********************
 * Environment
 ********************)

module VarMap = Map.Make(String)

type environment = {
  params: Sast.var VarMap.t;
  scope: Sast.var VarMap.t;
}

let builtins = VarMap.empty
let builtins = VarMap.add "EUL" { name = "EUL"; s_type = Num; } builtins
let builtins = VarMap.add "PI" { name = "PI"; s_type = Num; } builtins
let builtins = VarMap.add "print" {
  name = "print";
  s_type = Func({ param_types = [Unconst]; return_type = Void; });
} builtins

let root_env = {
  params = VarMap.empty;
  scope = builtins;
}


(********************
 * Utilities
 ********************)

let rec str_of_type = function
  | Num -> "Num"
  | String -> "String"
  | Bool -> "Bool"
  | Void -> "Void"
  | List(l) -> sprintf "List[%s]" (str_of_type l)
  | Func(f) -> str_of_func f
  | Unconst -> "Unconst"

and str_of_func f =
  let param_types = List.map str_of_type f.param_types and
    return_type = str_of_type f.return_type in
  sprintf "Func(%s => %s)" (String.concat ", " param_types) return_type

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

let print_env env =
  let print_var id var =
    let line = sprintf "\t%s --> { name: %s; s_type: %s; }"
    id var.name (str_of_type var.s_type) in
    print_endline line in
  let str_of_varmap name vm =
    let header = sprintf "%s:" name in
    print_endline header; VarMap.iter print_var vm in
  print_endline "";
  str_of_varmap "env params" env.params;
  str_of_varmap "env scope" env.scope


(********************
 * Exceptions
 ********************)

exception Semantic_Error of string

let var_error id =
  let message = sprintf "Use of variable '%s' is undefined in current scope" id
  in raise (Semantic_Error message)

let unop_error op t = 
  let message = sprintf "Invalid use of unary operator '%s' with type %s"
    (str_of_unop op) (str_of_type t) in
  raise (Semantic_Error message)

let binop_error t1 op t2 = 
  let message =
    sprintf "Invalid use of binary operator '%s' with type %s and %s" 
    (str_of_binop op) (str_of_type t1) (str_of_type t2) in
  raise (Semantic_Error message)

let fcall_error id f =
  let name = match id with
    | Sast.Id(name) -> name
    | _ -> raise 
        (Semantic_Error "Sast.Call provided non-ID as first argument") in
  let message = sprintf "Invalid call of function '%s' with type %s"
    name (str_of_func f) in
  raise (Semantic_Error message)

let assign_error id typ =
  let message = sprintf "Invalid assignment of id %s to type %s"
    id (str_of_type typ) in
  raise (Semantic_Error message)

let list_error list_type elem_type = 
  let message = sprintf "Invalid element of type %s in list of type %s"
    (str_of_type elem_type) (str_of_type list_type) in
  raise (Semantic_Error message)

let fdecl_unconst_error id =
  let message = sprintf
    "Invalid declaration of function '%s' with unconstrained return value" id in
  raise (Semantic_Error message)


(********************
 * Scoping
 ********************)

(* Variable counter to prevent naming conflicts *)
let ss_counter = ref (-1)

(* Given a string x, get a unique id x_# to use as the next variable *)
let get_ssid name =
  ss_counter := !ss_counter + 1;
  sprintf "%s_%d" name !ss_counter

(* Given an ssid my_var_#, return the original key ID my_var *)
let id_of_ssid ssid =
  let id_len = String.rindex ssid '_' in
  String.sub ssid 0 id_len

(* Add 'id' with type 's_type' to the environment scope *)
let add_to_scope env id s_type =
  let ss_id = get_ssid id in
  let var = { name = ss_id; s_type = s_type } in
  let env' = {
    params = env.params;
    scope = VarMap.add id var env.scope;
  } in
  env', ss_id

(*
 * Add param with 'id' and type Unconst to the environment params, erasing it
 * from the environment scope
 *)
let add_to_params env id =
  let ss_id = get_ssid id in 
  let var = { name = ss_id; s_type = Unconst } in
  let env' = {
    params = VarMap.add id var env.params;
    scope = VarMap.remove id env.scope;
  } in
  env', ss_id


(***********************************
 * Type inference and constraining
 ***********************************)

(* Update the type for given id corresponding to given 'ssid' in env *)
let update_type env ssid typ =
  let id = id_of_ssid ssid in
  if VarMap.mem id env.scope then (VarMap.find id env.scope).s_type <- typ else
  if VarMap.mem id env.params then (VarMap.find id env.params).s_type <- typ
  else var_error id

(* 
 * Attempt to constrain an ID in an expression one level down. E.g. !x would
 * constrain x to a boolean and x + y would constrain both x and y to integers,
 * but !(x == y) would not constrain either variable.
 *
 * Takes the current environment, type to constrain, and an expression wrapper
 * in which to search for an ID. Returns the newly constrained environment and
 * expression wrapper on success, or their old values on failure.
 *)
let constrain_ssid env ew typ =
  let Sast.Expr(e, _) = ew in
  match e with
    | Sast.Id(ssid) -> update_type env ssid typ; env, Sast.Expr(e, typ)
    | Sast.Fdecl(f) -> update_type env f.fname typ; env, Sast.Expr(e, typ)
    | _ -> env, ew


(* Type Inference/Type Constraints *)
(* This function looks to see if any of the constraints applied in the
 * child should be applied to any of the variables in the parent env *)
(*let add_constraints child_env parent_env =  
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
  VarMap.iter debugger p_scope; { parent_env with scope = p_scope }*)


(************************************************
 * Semantic checking and tree SAST construction
 ************************************************)

(* Branching point *)
let rec check_expr env = function
  | Ast.Num_lit(x) -> env, Sast.Expr(Sast.Num_lit(x), Num)
  | Ast.String_lit(s) -> env, Sast.Expr(Sast.String_lit(s), String)
  | Ast.Bool_lit(b) -> env, Sast.Expr(Sast.Bool_lit(b), Bool)
  | Ast.Void_lit -> env, Sast.Expr(Sast.Void_lit, Void)
  | Ast.Id(id) -> check_id env id
  | Ast.Unop(op, e) -> check_unop env op e
  | Ast.Binop(e1, op, e2) -> check_binop env e1 op e2
  | Ast.Call(id, args) -> check_func_call env id args
  | Ast.Assign(id, e) -> check_assign env id e
  | Ast.List(l) -> check_list env l
  | Ast.Fdecl(f) -> check_fdecl env "anon" f

(* Find string key 'id' in the environment if it exists *)
and check_id env id =
  let var =
    if VarMap.mem id env.scope then VarMap.find id env.scope else
    if VarMap.mem id env.params then VarMap.find id env.params else
    var_error id in
  env, Sast.Expr(Sast.Id(var.name), var.s_type)

(* Unary operators *)
and check_unop env op e =
  let env', ew = check_expr env e in
  let Sast.Expr(_, typ) = ew in
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

(* Binary operators *)
and check_binop env e1 op e2 =
  let env', ew1 = check_expr env e1 in
  let Sast.Expr(_, typ1) = ew1 in
  let env', ew2 = check_expr env' e2 in
  let Sast.Expr(_, typ2) = ew2 in
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
        (* No constraining can be done on overloaded equality operator *)
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

(* Function calling *)
and check_func_call env id args =
  let env', ew = check_expr env id in
  let Sast.Expr(id, typ) = ew in
  let f = match typ with
    | Sast.Func(f) -> f
    | _ -> raise (Semantic_Error "Attempting to call a non-function") in
  let args = check_func_call_args env' id f args in
  env', Sast.Expr(Sast.Call(ew, args), f.return_type)

and check_func_call_args env id f args =
  if List.length f.param_types <> List.length args then fcall_error id f else
  let rec aux acc param_types = function
    | [] -> List.rev acc
    | (Sast.Expr(_, typ) as ew) :: tl -> let const = List.hd param_types in
      (* TODO: constrain types if possible within function? *)
      if (typ = const || const = Unconst) then
        aux (ew :: acc) (List.tl param_types) tl
      else fcall_error id f in
  (* TODO: don't throw away env from args here *)
  aux [] f.param_types (List.map (fun e -> snd(check_expr env e)) args)

(* Assignment *)
and check_assign env id = function
  | Ast.Fdecl(f) -> check_fdecl env id f
  | _ as ew -> let env', ew' = check_expr env ew in
      let Sast.Expr(_, typ) = ew' in
      if typ = Void then assign_error id Void else
      let env', name = add_to_scope env' id typ in
      env', Sast.Expr(Sast.Assign(name, ew'), typ)

(* Lists *)
and check_list env l =
  if List.length l = 0 then env, Sast.Expr(Sast.List([]), List(Unconst)) else
  let rec aux acc const = function
    | [] -> Sast.Expr(Sast.List(List.rev acc), List(const))
    | (Sast.Expr(_, typ) as ew) :: tl ->
      (* TODO: constrain type to const *)
      if typ = const || const = Unconst then
        aux (ew :: acc) const tl
      else list_error (List(const)) typ in
  (* TODO: don't throw away env from args here *)
  let ew_list = List.map (fun e -> snd(check_expr env e)) l in
  let Sast.Expr(_, const) = List.hd ew_list in
  env, aux [] const ew_list

(* Function declaration *)
and check_fdecl env id f =
  (* Add function name to scope to allow recursion *)
  let env', name = add_to_scope env id Unconst in

  (* Evaluate parameters, body, and return statement in local environment *)
  let func_env, param_ssids = check_fdecl_params env' f.params in
  let func_env, body = check_stmts func_env f.body in
  let func_env, return = check_expr func_env f.return in
  let Sast.Expr(_, ret_type) = return in
  if ret_type = Unconst then fdecl_unconst_error id else

  (* Construct function declaration *)
  let fdecl = {
    fname = name;
    params = param_ssids;
    body = body;
    return = return;
  } in

  (* Evaluate parameter and function types *)
  let param_types = 
    let type_of_param ssid =
      let var = VarMap.find (id_of_ssid ssid) func_env.params in
      var.s_type in
    List.map type_of_param param_ssids in
  let f_type = Func({ param_types = param_types; return_type = ret_type }) in
  
  (* Update function type in environment and return expression wrapper *)
  let old_ew = Sast.Expr(Sast.Fdecl(fdecl), Unconst) in
  constrain_ssid env' old_ew f_type

and check_fdecl_params env param_list =
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | param :: tl -> let env', name = add_to_params env param in
        aux env' (name :: acc) tl
  in aux env [] param_list

(* Statements *)
and check_stmt env = function
  | Ast.Do(e) -> let env', ew = check_expr env e in env', Sast.Do(ew)

and check_stmts env stmt_list = 
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | stmt :: tl -> let env', e = check_stmt env stmt in
        aux env' (e :: acc) tl
  in aux env [] stmt_list

(* Program entry point *)
let check_ast ast = 
  let _, sast = check_stmts root_env ast in sast
