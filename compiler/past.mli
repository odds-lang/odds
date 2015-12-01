(*
 * COMS4115: Odds python abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Ast

(* Expressions *)
type expr = 
  | Num_lit of Ast.num
  | String_lit of string
  | Void_lit
  | Bool_lit of bool
  | Unop of Ast.unop * expr
  | Binop of expr * Ast.binop * expr
  | Id of string
  | Assign of string * expr
  | Call of expr * expr list
  | List of expr list

(* Function Declarations *)
and fdecl = {
  name: string;          (* Function Name *)
  params: string list;   (* Parameters *)
  body: stmt list;       (* Function Body *)
  return: expr;          (* Return *)
}

(* Statements *)
and stmt =
  | Do of expr     (* set foo = bar + 3 *)
  | Def of fdecl

(* Program entry point *)
type program = stmt list