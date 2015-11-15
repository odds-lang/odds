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
open Printf

module StringMap = Map.Make(String)

let ss_counter = ref (-1) (* Static Scoping Variable Counter *)

let get_ss_id name = 
  ss_counter := !ss_counter + 1;
  sprintf "%s_%d" name !ss_counter

(* Binary operators *)
let txt_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"
  | Not -> "not "
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

(* Conditionals *)
let txt_of_cond i t e = sprintf "if %s:\n{\n%s\n}\nelse:\n{\n%s\n}" i t e

(* Expressions *)
let rec txt_of_expr env = function
  | Int_lit(i) -> env, string_of_int(i)
  | Float_lit(f) -> env, string_of_float(f)
  | String_lit(s) -> env, sprintf "\"%s\"" s
  | Bool_lit(b) -> env, String.capitalize (string_of_bool(b))
  | Id(e) -> env, if StringMap.mem e env then StringMap.find e env else e
  | Unop(op, e) -> let _, e = txt_of_expr env e in 
      env, sprintf "(%s%s)" (txt_of_op op) e
  | Binop(e1, op, e2) -> 
      let _, e1 = txt_of_expr env e1 and _, e2 = txt_of_expr env e2 in
      env, sprintf "(%s %s %s)" e1 (txt_of_op op) e2
  | Call(f, args) -> let _, id = txt_of_expr env f in
      env, txt_of_func_call env id args
  | Assign(id, e) ->
      let ss_id = get_ss_id id and _, e = txt_of_expr env e in
      StringMap.add id ss_id env, sprintf "%s = %s" ss_id e
  | List(l) -> let e = txt_of_list env l in env, sprintf "[%s]" e
  | If(e1, e2, e3) ->
    let i = txt_of_expr e1 and t = txt_of_expr e2 and e = txt_of_expr e3 in
    txt_of_cond i t e

(* Function calls *)
and txt_of_func_call env f args = match f with
  | "print" -> sprintf "print(%s)" (txt_of_list env args)
  | _ ->  sprintf "%s(%s)" f (txt_of_list env args)

and txt_of_list env = function
  | [] -> ""
  | [x] -> snd (txt_of_expr env x)
  | _ as l -> String.concat ", " (List.map (fun e -> snd (txt_of_expr env e)) l)

(* Statements *)
let txt_of_stmt env = function
  | Do(e) -> let env, e = txt_of_expr env e in env, sprintf "%s" e

let txt_of_stmts stmt_list = 
  let rec process_stmts env acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | stmt :: tl -> let updated_env, stmt_txt = txt_of_stmt env stmt in
        process_stmts updated_env (stmt_txt :: acc) tl
  in process_stmts StringMap.empty [] stmt_list

(* Code generation entry point *)
let gen_program output_file program =
  let code = txt_of_stmts program in 
  let file = open_out output_file in
  fprintf file "%s\n" code; close_out file
