(*
 * COMS4115: Odds pretty printer for semantically checked abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)
open Ast
open Sast
open Analyzer
open Printf


let str_of_expr = function
  | Num_lit(n) -> 
      begin match n with
        | Ast.Num_int(i) -> string_of_int i
        | Ast.Num_float(f) -> string_of_float f
      end
  | String_lit(s) -> s
  | Bool_lit(b) -> string_of_bool b
  | Unop(op, we) -> 
    let op_str = Sast.str_of_unop op and we_str = str_of_wrapped_expr we in
    sprintf "%s%s" op_str we_str
  | Binop(we1, op, we2) -> 
      let we1_str = str_of_wrapped_expr we1 and
        op_str = Sast.str_of_unop op and 
        we2_str = str_of_wrapped_expr we2 in
      sprintf "%s %s %s" we1_str op_str we2_str
  | Id(id) -> id
  | Assign(id, we) -> 
      let we_str = str_of_wrapped_expr we in 
      sprintf "%s = %s" id we_str
  | Call(we, we_list) -> 
  | List(we_list) -> 
  | Fdecl(fdecl) -> 

and str_of_wrapped_expr = function
  | Sast.Expr(expr, typ) -> 
      let type_str = Analyzer.str_of_type typ and expr_str = str_of_expr expr in
      sprintf "{%s %s}" type_str expr_str

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
