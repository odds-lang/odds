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
open Past
open Printf

(* Indentation *)
let indent_of_num indent = String.make indent '\t'

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
let rec txt_of_expr indent = function
  | Num_lit(n) -> txt_of_num n
  | String_lit(s) -> sprintf "\"%s\"" s
  | Bool_lit(b) -> String.capitalize (string_of_bool(b))
  | None_lit -> "None"
  | Id(id) -> id
  | Unop(op, e) -> sprintf "(%s%s)"
      (txt_of_unop op)
      (txt_of_expr indent e)
  | Binop(e1, op, e2) -> sprintf "(%s %s %s)"
      (txt_of_expr indent e1)
      (txt_of_binop op)
      (txt_of_expr indent e2)
  | Call(id, args) -> sprintf "%s(%s)"
      (txt_of_expr indent id) (txt_of_list indent args)
  | Assign(id, e) -> sprintf "%s = %s" id (txt_of_expr indent e)
  | List(l) -> sprintf "[%s]" (txt_of_list indent l)
  | Def(f) -> txt_of_fdecl indent f

(* Lists *)
and txt_of_list indent = function
  | [] -> ""
  | [x] -> txt_of_expr indent x
  | _ as l ->
    let strs = List.map (fun x -> txt_of_expr indent x) l
    in String.concat ", " strs

(* Functions *)
and txt_of_fdecl indent f =
    let params = String.concat ", " f.p_params in
    let body = txt_of_stmts (indent + 1) f.p_body in
    let return = txt_of_expr indent f.p_return in
    sprintf "def %s(%s):\n\n%s\n%sreturn %s\n"
      f.p_name
      params
      body
      (indent_of_num (indent+1))
      return

(* Statements *)
and txt_of_stmt indent = function
  | Past.Stmt(e) -> sprintf "%s%s"
      (indent_of_num indent)
      (txt_of_expr indent e)

and txt_of_stmts indent stmt_list =
  let rec aux indent acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | stmt :: tl -> aux indent ((txt_of_stmt indent stmt) :: acc) tl
  in aux indent [] stmt_list

(* Code generation entry point *)
let gen_program output_file past =
  let txt = txt_of_stmts 0 past in
  let file = open_out output_file in
    fprintf file "%s\n" txt; close_out file
