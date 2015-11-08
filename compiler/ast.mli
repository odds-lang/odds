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
  | Id of string                 (* x *)
  | Binop of expr * op * expr    (* a + b *)
  | Unop of op * expr            (* -5 *)
  | Call of string * expr list   (* add(1, 2) *)

type stmt =          (* Statements *)
  | State of expr    (* set foo = bar + 3 *)

type program = stmt list

