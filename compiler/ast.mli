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

type expr =                      (* Expressions *)
  | Int_lit of int               (* 42 *)
  | Float_lit of float           (* 42.0 *)
  | String_lit of string         (* "Hello, world" *)
  | Bool_lit of bool             (* true *)
  | Unop of op * expr            (* -5 *)
  | Binop of expr * op * expr    (* a + b *)
  | Id of string                 (* x *)
  | Assign of expr * expr      (* x = 4 *)
  | Call of expr * expr list   (* add(1, 2) *)
  | Fdecl of fdecl               (* (x) -> ... return x *)

and fdecl =               (* Function Declarations *)
  {
    params: expr list;    (* Parameters *)
    body: stmt list;      (* Function Body *)
    return: expr          (* Return *)
  }

and stmt =         (* Statements *)
  | Do of expr     (* set foo = bar + 3 *)


type program = stmt list
