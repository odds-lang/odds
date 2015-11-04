(*
 * COMS4115: Odds abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

type op =    (* Operators *)
  | Add      (* + *)
  | Sub      (* - *)
  | Mult     (* * *)
  | Div      (* / *)

type expr =                      (* Expressions *)
  | Literal of int               (* 42 *)
  | Binop of expr * op * expr    (* a + b *)

type stmt =         (* Statements *)
  | Expr of expr    (* set foo = bar + 3 *)

type program = stmt
