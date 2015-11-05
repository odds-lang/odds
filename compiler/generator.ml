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

(* WILL REMOVE AND ADD TO SAST *)
let func_to_text f arg = match f with
  | "print" -> sprintf "print(%s)" arg
  | _ -> "" (* f(args) *)

(* WILL REMOVE AND ADD TO SAST *)
let op_to_text op = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"

(* WILL REMOVE AND ADD TO SAST *)
let rec exp_to_text exp = match exp with
  | Int_lit(i) -> string_of_int(i)
  | Float_lit(f) -> string_of_float(f)
  | String_lit(s) -> sprintf "\"%s\"" s
  | Binop(e1, op, e2) ->
      sprintf "%s %s %s" (exp_to_text e1) (op_to_text op) (exp_to_text e2)
  | Unop(op, e) -> sprintf "%s %s" (op_to_text op) (exp_to_text e)

(* write program to a .py file *)
let py_file = open_out "out.py"

(* entry point for code generation *)
let rec process_stmt_list stmt_list = match stmt_list with
  | stmt :: remaining_stmts -> ignore(process_stmt stmt);
      process_stmt_list remaining_stmts
  | [] -> close_out py_file

and process_stmt stmt = match stmt with
  | State(expr) -> ""

