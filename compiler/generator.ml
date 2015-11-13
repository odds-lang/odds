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

(*
let get_pyname =
  let get_str num = sprintf "%c" (Char.chr (97 + num)) in 
  let rec aux str num =
    if num < 10 then get_str num
    else sprintf "%s%s" (aux str (num / 10)) (get_str (num mod 10))
  in aux "" !cur_pyname
*)

let get_ss_id name = 
  ss_counter := !ss_counter + 1;
  sprintf "%s_%d" name !ss_counter

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

let rec txt_of_expr = function
  | Int_lit(i) -> string_of_int(i)
  | Float_lit(f) -> string_of_float(f)
  | String_lit(s) -> sprintf "\"%s\"" s
  | Bool_lit(b) -> String.capitalize (string_of_bool(b))
  | Id(e) -> e
  | Unop(op, e) -> sprintf "(%s%s)" (txt_of_op op) (txt_of_expr e)
  | Binop(e1, op, e2) ->
      sprintf "(%s %s %s)" (txt_of_expr e1) (txt_of_op op) (txt_of_expr e2)
  | Call(f, args) -> txt_of_func_call (txt_of_expr f) args

and txt_of_func_call f args = match f with
  | "print" -> sprintf "print(%s)" (txt_of_args args)
  | _ ->  sprintf "%s(%s)" f (txt_of_args args)

and txt_of_args = function
  | [] -> ""
  | [arg] -> txt_of_expr arg
  | _ as arg_list -> String.concat ", " (List.map txt_of_expr arg_list)

let process_stmt = function
  | Do(expr) -> sprintf "%s" (txt_of_expr expr)

let process_stmts stmt_list = 
  let rec aux acc = function
    | [] -> String.concat "\n" acc
    | stmt :: tl -> aux (process_stmt stmt :: acc ) tl
  in aux [] stmt_list

(* entry point for code generation *)
let gen_program output_file program =
  let code = process_stmts program in 
  let file = open_out output_file in
  fprintf file "%s\n" code; close_out file
