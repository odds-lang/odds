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

let rec txt_of_exp exp = match exp with
  | Int_lit(i) -> string_of_int(i)
  | Float_lit(f) -> string_of_float(f)
  | String_lit(s) -> sprintf "\"%s\"" s
  | Id(e) -> e
  | Binop(e1, op, e2) ->
      sprintf "%s %s %s" (txt_of_exp e1) (txt_of_op op) (txt_of_exp e2)
  | Unop(op, e) -> sprintf "%s %s" (txt_of_op op) (txt_of_exp e)
  | _ -> ""

and txt_of_func f arg = match f with
  | "print" -> sprintf "print(%s)" arg
  | _ -> "" (* f(args) *)

and txt_of_args arg_list = 
    String.concat ", " (List.map txt_of_exp arg_list)

and txt_of_op op = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"

(* write program to a .py file *)
let py_file = open_out "out.py"

(* entry point for code generation *)
let rec process_stmt_list stmt_list = match stmt_list with
  | stmt :: remaining_stmts -> ignore(process_stmt stmt);
      process_stmt_list remaining_stmts
  | [] -> close_out py_file

and process_stmt stmt = match stmt with
  | State(expr) -> ""

