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

(* Utility Functions *)
let tabs = ref 0
let tab_str () = String.make (!tabs * 2) ' '

(* Stringerizer *)
let rec str_of_expr = function
  | Num_lit(n) -> 
      begin match n with
        | Ast.Num_int(i) -> string_of_int i
        | Ast.Num_float(f) -> string_of_float f
      end
  | String_lit(s) -> s
  | Bool_lit(b) -> string_of_bool b
  | Void_lit -> "()"
  | Unop(op, we) -> 
      let op_str = Analyzer.str_of_unop op and we_str = str_of_wrapped_expr we in
      sprintf "%s%s" op_str we_str
  | Binop(we1, op, we2) -> 
      let we1_str = str_of_wrapped_expr we1 and
        op_str = Analyzer.str_of_binop op and 
        we2_str = str_of_wrapped_expr we2 in
      sprintf "%s %s %s" we1_str op_str we2_str
  | Id(id) -> id
  | Assign(id, we) -> 
      let we_str = str_of_wrapped_expr we in 
      sprintf "%s = %s" id we_str
  | Call(we, we_list) -> 
      let func_name = str_of_wrapped_expr we and 
        args_txt = str_of_wrapped_expr_list we_list in
      sprintf "%s(%s)" func_name args_txt
  | Ldecl(we_list) -> 
      let l_txt = str_of_wrapped_expr_list we_list in
      sprintf "[%s]" l_txt
  | Fdecl(fdecl) -> 
      tabs := !tabs + 1;
      let params_txt = String.concat ", " fdecl.params and
        body_txt = str_of_stmts fdecl.body and
        return_txt = str_of_wrapped_expr fdecl.return in
      let f_str = sprintf "%s(%s) ->\n%s\n%sreturn %s\n" fdecl.f_name params_txt 
        body_txt (tab_str ()) return_txt in
      tabs := !tabs - 1; f_str
  | Cake(fdecl, call) ->
      tabs := !tabs + 1;
      let fdecl_txt = str_of_wrapped_expr fdecl and
        call_txt = str_of_wrapped_expr call in
      sprintf "{{\n%s\n}}\n{{\n%s\n}}" fdecl_txt call_txt
  | If(cond) -> sprintf "%s" (str_of_cond cond)

and str_of_cond cond =
    sprintf "if (%s)\n  %s\n else  %s" 
      (str_of_wrapped_expr cond.cond)
      (str_of_wrapped_expr cond.stmt_1)
      (str_of_wrapped_expr cond.stmt_2)

and str_of_wrapped_expr_list l = 
  String.concat ", " (List.map str_of_wrapped_expr l)
  
and str_of_wrapped_expr = function
  | Sast.Expr(expr, typ) -> 
      let type_str = Analyzer.str_of_type typ and expr_str = str_of_expr expr in
      sprintf "%s %s" type_str expr_str

and str_of_stmt = function
  | Sast.Do(wrapped_expr) -> 
      sprintf "%sdo %s" (tab_str ()) (str_of_wrapped_expr wrapped_expr)

and str_of_stmts sast = 
  let rec aux acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | hd :: tl -> aux (str_of_stmt hd :: acc) tl
  in aux [] sast

let print_sast sast =
    let sast_str = str_of_stmts sast in
    print_endline sast_str