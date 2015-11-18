(*
 * COMS4115: Odds parser
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

let ss_counter = ref (-1) (* Static Scoping Variable Counter *)

let get_ss_id name =
  ss_counter := !ss_counter + 1;
  sprintf "%s_%d" name !ss_counter

(* Unary operators *)
let txt_of_unop = function
  | Not -> "not "
  | Sub -> "-"

(* Binary operators *)
let txt_of_num = function
  | Num_int(i) -> string_of_int i
  | Num_float(f) -> string_of_float f

let txt_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

(* Identifier lookup *)
let txt_of_id env = function
  | "PI" -> string_of_float 3.14159265
  | "EUL" -> string_of_float 2.7182818
  | "print" -> "print"
  | _ as id -> if StringMap.mem id env then StringMap.find id env
      else raise Not_found

(* Expressions *)
let rec txt_of_expr env = function
  | Num_lit(n) -> env, txt_of_num n
  | String_lit(s) -> env, sprintf "\"%s\"" s
  | Bool_lit(b) -> env, String.capitalize (string_of_bool(b))
  | Id(id) -> env, txt_of_id env id
  | Unop(op, e) -> let _, e = txt_of_expr env e in 
      env, sprintf "(%s%s)" (txt_of_unop op) e
  | Binop(e1, op, e2) ->
      let _, e1 = txt_of_expr env e1 and _, e2 = txt_of_expr env e2 in
      env, sprintf "(%s %s %s)" e1 (txt_of_binop op) e2
  | Call(f, args) -> let _, id = txt_of_expr env f in
      env, sprintf "%s(%s)" id (txt_of_list env args)
  | Assign(id, e) -> txt_of_assign env id e
  | List(l) -> let e = txt_of_list env l in env, sprintf "[%s]" e
  | Fdecl(f) -> txt_of_fdecl env "anon" f

and txt_of_list env = function
  | [] -> ""
  | [x] -> snd (txt_of_expr env x)
  | _ as l -> String.concat ", " (List.map (fun e -> snd (txt_of_expr env e)) l)

and txt_of_fdecl env id f =
    let ss_id = get_ss_id id in
    let new_env = StringMap.add id ss_id env in
    let list_txt = txt_of_list new_env f.params in
    let ret_env, body_txt = txt_of_stmts new_env f.body in
    let _, ret_txt = txt_of_expr ret_env f.return in
    let func_txt = sprintf "def %s(%s):\n{\n%s\nreturn %s\n}" 
      ss_id list_txt body_txt ret_txt in
    new_env, func_txt

and txt_of_assign env id = function
  | Fdecl(f) -> txt_of_fdecl env id f
  | _ as e ->
      let ss_id = get_ss_id id and _, e = txt_of_expr env e in
      StringMap.add id ss_id env, sprintf "%s = %s" ss_id e

(* Statements *)
and txt_of_stmt env = function
  | Sast.Do(e) -> let env, e = txt_of_expr env e in env, sprintf "%s" e

and txt_of_stmts env_input stmt_list =
  let rec process_stmts env acc = function
    | [] -> env, String.concat "\n" (List.rev acc)
    | stmt :: tl -> let updated_env, stmt_txt = txt_of_stmt env stmt in
        process_stmts updated_env (stmt_txt :: acc) tl
  in process_stmts env_input [] stmt_list

(* Code generation entry point *)
let gen_program output_file program =
  let _, text = txt_of_stmts StringMap.empty program in
  let file = open_out output_file in
  fprintf file "%s\n" text; close_out file
