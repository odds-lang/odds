(*
 * COMS4115: Odds pretty printer for semantically checked abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)
open Sast
open Analyzer
open Printf


let rec str_of_expr = function
  | Num_lit(n) -> 
  | String_lit(s)
  | Bool_lit(b)
  | Unop(op, wrapped_expr)
  | Binop of expr * Ast.binop * expr
  | Id of string
  | Assign of string * expr
  | Call of expr * expr list
  | List of expr list
  | Fdecl of fdecl

and str_of_wrapped_expr = function
  | Sast.Expr(expr, typ) -> 
      let type_str = Analyzer.str_of_type typ and expr_str = str_of_expr expr in
      sprintf "%s %s" type_str expr_str

let str_of_stmt = function
  | Sast.Do(expr_wrapper) -> 
      sprintf "do %s" (txt_of_wrapped_expr wrapped_expr)

let txt_of_stmts sast = 
  let rec aux acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | hd :: tl -> aux (txt_of_stmt hd :: acc) tl
  in aux [] sast

let print sast = 
  let sast_str = txt_of_stmts sast in
  print_string sast_str
