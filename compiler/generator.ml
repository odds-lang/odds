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

let func_to_text f arg = match f with
  | "print" -> sprintf "print(\"%s\")" arg
  | _ -> "" (* f(args) *)

let op_to_text op = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"

let py_file = open_out "out.py"

in let rec process_stmt_list stmt_list = match stmt_list with
  | stmt :: remaining_stmts -> ignore(process_stmt stmt);
      process_stmt_list remaining_stmts
  | [] -> close_out py_file

and process_stmt stmt = match stmt with
  | State(expr) -> generate_expr(expr)

