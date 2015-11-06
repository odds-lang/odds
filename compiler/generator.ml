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

let rec txt_of_expr expr = match expr with
  | Int_lit(i) -> string_of_int(i)
  | Float_lit(f) -> string_of_float(f)
  | String_lit(s) -> sprintf "\"%s\"" s
  | Id(e) -> e
  | Binop(e1, op, e2) ->
      sprintf "%s %s %s" (txt_of_expr e1) (txt_of_op op) (txt_of_expr e2)
  | Unop(op, e) -> sprintf "%s %s" (txt_of_op op) (txt_of_expr e)
  | Call(f, args) -> txt_of_func_call f args

and txt_of_func_call f args = match f with
  | "print" -> sprintf "print(%s)" (txt_of_args args)
  | _ ->  sprintf "%s(%s)" f (txt_of_args args)

and txt_of_args arg_list = match arg_list with
  | [] -> ""
  | [arg] -> txt_of_expr arg
  | _ -> String.concat ", " (List.map txt_of_expr arg_list)

and txt_of_op op = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"

(* entry point for code generation *)
let rec process_stmt_list stmt_list prog_list = match stmt_list with
  | stmt :: remaining_stmts -> 
      process_stmt_list remaining_stmts (process_stmt stmt :: prog_list)
  | [] -> String.concat "\n" (List.rev prog_list)

and process_stmt stmt = match stmt with
  | State(expr) -> sprintf "%s" (txt_of_expr expr)

and writeToFile file_name prog_string =
  let file = open_out (file_name ^ ".py") in
    fprintf file "%s" prog_string
    
and gen_program file_name prog =
  let pythonString = process_stmt_list prog [] in 
  writeToFile file_name pythonString;;

