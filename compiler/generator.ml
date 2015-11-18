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
  | Id(id) -> id
  | Unop(op, e) -> sprintf "(%s%s)" (txt_of_unop op) (txt_of_expr e)
  | Binop(e1, op, e2) ->
      sprintf "(%s %s %s)" (txt_of_expr e1) (txt_of_binop op) (txt_of_expr e2)
  | Call(id, args) -> sprintf "%s(%s)" (txt_of_expr id) (txt_of_list args)
  | Assign(id, e) -> sprintf "%s = %s" id (txt_of_expr e)
  | List(l) -> sprintf "[%s]" (txt_of_list l)
  | Fdecl(f) -> txt_of_fdecl f

and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> String.concat ", " (List.map txt_of_expr l)

and txt_of_fdecl f =
    let params = txt_of_list f.params in
    let body = txt_of_stmts f.body in
    let return = txt_of_expr f.return in
    sprintf "def %s(%s):\n{\n%s\nreturn %s\n}" f.name params body return

(* Statements *)
and txt_of_stmt = function
  | Sast.Do(e) -> sprintf "%s" (txt_of_expr e)

and txt_of_stmts stmt_list =
  let rec aux acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | stmt :: tl -> aux (txt_of_stmt stmt :: acc) tl
  in aux [] stmt_list

(* Code generation entry point *)
let gen_program output_file program =
  let text = txt_of_stmts program in
  let file = open_out output_file in
  fprintf file "%s\n" text; close_out file
