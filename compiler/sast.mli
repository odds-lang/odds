(*
 * COMS4115: Odds semantically checked abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Ast

(* Data types *)
type data_type = 
  | Num
  | String
  | Bool
  | Void
  | List of data_type
  | Func of func
  | Any
  | Dist of dist
  | Unconst

and func = {
  param_types: data_type list;
  return_type: data_type;
}

type var = {
  name: string;
  mutable s_type: data_type;
}

(* Expressions *)
type expr_wrapper = 
  | Expr of expr * data_type

and expr = 
  | Num_lit of Ast.num
  | String_lit of string
  | Bool_lit of bool
  | Void_lit
  | Unop of Ast.unop * expr_wrapper
  | Binop of expr_wrapper * Ast.binop * expr_wrapper
  | Id of string
  | Assign of string * expr_wrapper
  | Call of expr_wrapper * expr_wrapper list
  | Dist of dist
  | Ldecl of expr_wrapper list
  | Fdecl of fdecl
  | If of ifdecl

and ifdecl = {
  c_name: string;
  cond: expr_wrapper;
  stmt_1: expr_wrapper;
  stmt_2: expr_wrapper;
}

and dist = {
  min: expr_wrapper;        (* Distribution Minimum *)
  max: expr_wrapper;        (* Distribution Maximum *)
  dist_func: expr_wrapper;  (* Distribution Function *)
}

and fdecl = {
  f_name: string;         (* Function Name *)
  params: string list;    (* Parameters *)
  body: stmt list;        (* Function Body *)
  return: expr_wrapper;   (* Return *)
  is_anon: bool;          (* Anonymous? *)
}

(* Statements *)
and stmt =
  | Do of expr_wrapper    (* set foo = bar + 3 *)

(* Program entry point *)
type program = stmt list
