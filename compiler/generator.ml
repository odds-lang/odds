(*
 * COMS4115: Odds Python code generator
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

exception Python_Error of string

(* Indentation *)
let indent_of_num indent = String.make (4 * indent) ' '

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
  | Or -> "or"
  | And -> "and"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

(* Conditionals *)
let txt_of_cond indent i t e = sprintf "%sif %s:\n%s\n%s" 
  (indent_of_num indent) i t e

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
  | Ldecl(l) -> sprintf "[%s]" (txt_of_list indent l)
  | Empty -> ""

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
  sprintf "%sdef %s(%s):%s"
    (indent_of_num indent)
    f.p_name
    params
    (if String.length body > 0 then "\n" ^ body else "")

(* Statements *)
and txt_of_stmt indent = function 
  | Assign(id, e) -> sprintf "%s%s = %s" 
      (indent_of_num indent) id (txt_of_expr indent e)
  | Def(f) -> txt_of_fdecl indent f 
  | Return(e) -> sprintf "%sreturn %s" 
      (indent_of_num indent) (txt_of_expr indent e)
  | If(e1, e2, e3) -> 
      let i = txt_of_expr indent e1
      and t = txt_of_stmt (indent + 1) e2 
      and e = txt_of_stmt indent e3 in
      txt_of_cond indent i t e
  | Stmt(e) -> sprintf "%s%s" (indent_of_num indent) (txt_of_expr indent e)

and txt_of_stmts indent stmt_list =
  let rec aux indent acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | stmt :: tl -> aux indent ((txt_of_stmt indent stmt) :: acc) tl
  in aux indent [] stmt_list

(* Code generation entry point *)
let gen_program past = txt_of_stmts 0 past
