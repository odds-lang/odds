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

exception Printer_Error of string

(* Utility Functions *)
let tabsize = 2
let tabs = ref 0
let tab_str () = String.make (!tabs * tabsize) ' '

let str_of_colored_type typ = 
  sprintf "\x1b[34m%s\x1b[0m" (Analyzer.str_of_type typ)

(* Stringerizer *)
let rec str_of_expr_wrapper = function
  | Sast.Expr(Num_lit(n), _) -> 
      begin match n with
        | Ast.Num_int(i) -> string_of_int i
        | Ast.Num_float(f) -> string_of_float f
      end
  | Sast.Expr((String_lit(s)), _) -> sprintf "\"%s\"" s
  | Sast.Expr(Bool_lit(b), _) -> string_of_bool b
  | Sast.Expr(Void_lit, _) -> "void"
  | Sast.Expr(Unop(op, we), _) -> 
      let op_str = Analyzer.str_of_unop op and 
        we_str = str_of_expr_wrapper we in
      sprintf "%s%s" op_str we_str
  | Sast.Expr(Binop(we1, op, we2), _) -> 
      let we1_str = str_of_expr_wrapper we1 and
        op_str = Analyzer.str_of_binop op and 
        we2_str = str_of_expr_wrapper we2 in
      sprintf "%s %s %s" we1_str op_str we2_str
  | Sast.Expr(Id(id), typ) -> let typ_str = str_of_colored_type typ in
      sprintf "%s %s" typ_str id
  | Sast.Expr(Assign(id, we), typ) -> 
      let we_str = str_of_expr_wrapper we and 
        typ_str = str_of_colored_type typ in 
      sprintf "%s %s = %s" typ_str id we_str
  | Sast.Expr(Call(we, we_list), _) -> 
      let func_name = str_of_expr_wrapper we and 
        args_txt = str_of_expr_wrapper_list we_list in
      sprintf "%s(%s)" func_name args_txt
  | Sast.Expr(Ldecl(we_list), _) -> 
      let l_txt = str_of_expr_wrapper_list we_list in
      sprintf "[%s]" l_txt
  | Sast.Expr(Dist(dist), _) -> 
      let min_txt = str_of_expr_wrapper dist.min and
        max_txt = str_of_expr_wrapper dist.max and
        func_txt = str_of_expr_wrapper dist.dist_func in
      sprintf "<%s, %s>|%s|" min_txt max_txt func_txt
  | Sast.Expr(Fdecl(fdecl), typ) -> str_of_fdecl fdecl typ
  | Sast.Expr(Cake(fdecl_ew, call_ew), _) -> str_of_cake fdecl_ew call_ew
  | Sast.Expr(If(cond), _) -> str_of_cond cond

and str_of_expr_wrapper_list l = 
  String.concat ", " (List.map str_of_expr_wrapper l)

and str_of_fdecl fdecl typ = 
  let str_of_param_and_type typ param = 
    sprintf "%s %s" (str_of_colored_type typ) param in

  let func = match typ with
    | Func(func) -> func
    | _ -> raise (Printer_Error "Function has non-function type") in

  tabs := !tabs + 1;
  let params_and_types = List.map2 str_of_param_and_type func.param_types fdecl.params and
    return_type = str_of_colored_type func.return_type in
  
  let decl_txt = sprintf "%s => %s" (String.concat ", " params_and_types) return_type and
    body_txt = str_of_stmts fdecl.body and
    return_txt = str_of_expr_wrapper fdecl.return in
  
  let f_str = 
    let is_body = String.length body_txt > 0 in
    sprintf "%s(%s) ->%s%sreturn %s" fdecl.f_name decl_txt
      (if is_body then "\n" ^ body_txt ^ "\n" else "")
      (if is_body then tab_str () else " ")
      (if is_body then return_txt ^ "\n" else return_txt) in
  tabs := !tabs - 1; f_str

(* Currently not using *)
and str_of_cake fdecl_ew call_ew = 
  tabs := !tabs + 1;
  
  let fdecl, f_typ = match fdecl_ew with
    | Sast.Expr(Sast.Fdecl(fdecl), f_typ) -> fdecl, f_typ
    | _ -> raise (Printer_Error "Dead Code Path") in
  
  let fdecl_txt = str_of_fdecl fdecl f_typ and
    call_txt = str_of_expr_wrapper call_ew in
  let c_str = sprintf "{%s}%s" fdecl_txt call_txt in
  tabs := !tabs - 1; c_str

and str_of_cond cond =
  tabs := !tabs + 1;
  let tabins = (tab_str ()) ^ (String.make tabsize ' ') in
  
  let cond_str = sprintf "\n%sif %s then\n%s%s\n%selse\n%s%s" 
    (tab_str ()) (str_of_expr_wrapper cond.cond) tabins 
    (str_of_expr_wrapper cond.stmt_1) (tab_str ()) tabins 
    (str_of_expr_wrapper cond.stmt_2) in
  tabs := !tabs - 1; cond_str

and str_of_stmt = function
  | Sast.Do(wrapped_expr) -> 
      sprintf "%sdo %s" (tab_str ()) (str_of_expr_wrapper wrapped_expr)

and str_of_stmts sast = 
  let rec aux acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | hd :: tl -> aux (str_of_stmt hd :: acc) tl
  in aux [] sast

let print_sast sast =
    let sast_str = str_of_stmts sast in
    print_endline sast_str
