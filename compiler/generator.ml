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

module StringMap = Map.Make(String)

let cur_pyname = ref 0 (* Python variable name tracker *)

(*
let get_pyname =
  let get_str num = sprintf "%c" (Char.chr (97 + num)) in 
  let rec aux str num =
    if num < 10 then get_str num
    else sprintf "%s%s" (aux str (num / 10)) (get_str (num mod 10))
  in aux "" !cur_pyname
*)

let get_pyname name = sprintf "%s_%d" name !cur_pyname

let update_pyname () = cur_pyname := !cur_pyname + 1

let txt_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"

let rec txt_of_expr = function
  | Int_lit(i) -> string_of_int(i)
  | Float_lit(f) -> string_of_float(f)
  | String_lit(s) -> sprintf "\"%s\"" s
  | Id(id) -> id (* map *)
  | Binop(e1, op, e2) ->
      sprintf "(%s %s %s)" (txt_of_expr e1) (txt_of_op op) (txt_of_expr e2)
  | Unop(op, e) -> sprintf "(%s%s)" (txt_of_op op) (txt_of_expr e)
  | Call(f, args) -> txt_of_func_call f args

and txt_of_func_call f args = match f with
  | "print" -> sprintf "print(%s)" (txt_of_args args)
  | _ ->  sprintf "%s(%s)" f (txt_of_args args)

and txt_of_args = function
  | [] -> ""
  | [arg] -> txt_of_expr arg
  | _ as arg_list -> String.concat ", " (List.map txt_of_expr arg_list)

let process_stmt = function
  | State(expr) -> sprintf "%s" (txt_of_expr env expr)
  (*| Set(id, expr) -> sprinf "%s = %s" (* map *) (txt_of_expr env expr) *)

let rec process_stmts acc = function
  | [] -> String.concat "\n" acc
  | stmt :: tl -> process_stmts (process_stmt stmt :: acc env) tl

(* entry point for code generation *)
let gen_program output_file program =
  let code = process_stmts [] program in 
  let file = open_out output_file in
  fprintf file "%s\n" code; close_out file
