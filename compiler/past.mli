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
  | None_lit
  | Bool_lit of bool
  | Unop of Ast.unop * expr
  | Binop of expr * Ast.binop * expr
  | Id of string
  | Assign of string * expr
  | Call of expr * expr list
  | List of expr list
  | Def of fdecl

(* Function Declarations *)
and fdecl = {
  p_name: string;          (* Function Name *)
  p_params: string list;   (* Parameters *)
  p_body: stmt list;       (* Function Body *)
  p_return: expr;          (* Return *)
}

(* Statements *)
and stmt =
  | Stmt of expr     (* set foo = bar + 3 *)

(* Program entry point *)
type program = stmt list
