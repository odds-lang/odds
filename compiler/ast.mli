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
  | Eq       (* == *)
  | Neq      (* != *)
  | Less     (* < *)
  | Leq      (* <= *)
  | Greater  (* > *)
  | Geq      (* >= *)


type stmt =         (* Statements *)
  | Do of expr    (* set foo = bar + 3 *)

and fdecl =
  {
  params : expr list;
  body: stmt list;
  return: expr
  }

and expr =                      (* Expressions *)
  | Int_lit of int               (* 42 *)
  | Float_lit of float           (* 42.0 *)
  | String_lit of string         (* "Hello, world" *)
  | Bool_lit of bool             (* true *)
  | Id of string                 (* x *)
  | Unop of op * expr            (* -5 *)
  | Binop of expr * op * expr    (* a + b *)
  | Call of string * expr list   (* add(1, 2) *)
  | Assign of string * expr      (* x = 4 *)
  | Fdecl of fdecl  

type program = stmt list
