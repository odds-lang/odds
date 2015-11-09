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
  | Not      (* ! *)
  | Equal    (* == *)
  | Neq      (* != *)
  | Less     (* < *)
  | Leq      (* <= *)
  | Greater  (* > *)
  | Geq      (* >= *)

type expr =                      (* Expressions *)
  | Int_lit of int               (* 42 *)
  | Float_lit of float           (* 42.0 *)
  | String_lit of string         (* "Hello, world" *)
  | Bool_lit of bool             (* true *)
  | Id of string                 (* x *)
  | Unop of op * expr            (* -5 *)
  | Binop of expr * op * expr    (* a + b *)
  | Call of string * expr list   (* add(1, 2) *)

type stmt =          (* Statements *)
  | State of expr    (* set foo = bar + 3 *)

type program = stmt list
