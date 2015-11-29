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

(* Expressions *)
let rec txt_of_expr = function
  | Num_lit(n) -> txt_of_num n
  | String_lit(s) -> sprintf "\"%s\"" s
  | Bool_lit(b) -> String.capitalize (string_of_bool(b))
  | Void_lit -> "None"
  | Id(id) -> id
  | Unop(op, ew) -> sprintf "(%s%s)" (txt_of_unop op) (txt_of_expr_wrapper ew)
  | Binop(ew1, op, ew2) -> sprintf "(%s %s %s)"
      (txt_of_expr_wrapper ew1) (txt_of_binop op) (txt_of_expr_wrapper ew2)
  | Call(id, args) -> sprintf "%s(%s)"
      (txt_of_expr_wrapper id) (txt_of_list args)
  | Assign(id, e) -> sprintf "%s = %s" id (txt_of_expr_wrapper e)
  | List(l) -> sprintf "[%s]" (txt_of_list l)
  | Fdecl(f) -> txt_of_fdecl f

and txt_of_expr_wrapper = function
  | Expr(e, _) -> txt_of_expr e

and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr_wrapper x
  | _ as l -> String.concat ", " (List.map txt_of_expr_wrapper l)

and txt_of_fdecl f =
    let params = String.concat ", " f.params in
    let body = txt_of_stmts f.body in
    let return = txt_of_expr_wrapper f.return in
    sprintf "def %s(%s):\n{\n%s\nreturn %s\n}" f.fname params body return

(* Statements *)
and txt_of_stmt = function
  | Sast.Do(ew) -> sprintf "%s" (txt_of_expr_wrapper ew)

and txt_of_stmts stmt_list =
  let rec aux acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | stmt :: tl -> aux (txt_of_stmt stmt :: acc) tl
  in aux [] stmt_list

(* Code generation entry point *)
let gen_program output_file sast =
  let text = txt_of_stmts sast in
  let file = open_out output_file in
  fprintf file "%s\n" text; close_out file
