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
  | Mod      (* % *)
  | Pow      (* ** *)

type expr =                      (* Expressions *)
  | Int_lit of int               (* 42 *)
  | Float_lit of float           (* 42.0 *)
  | String_lit of string         (* "Hello, world" *)
  | Binop of expr * op * expr    (* a + b *)
  | Unop of op * expr

type stmt =         (* Statements *)
  | Expr of expr    (* set foo = bar + 3 *)

type program = stmt list
