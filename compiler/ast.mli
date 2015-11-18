(*
 * COMS4115: Odds abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

(* Unary operators *)
type unop =
  | Sub      (* - *) 
  | Not      (* ! *)

(* Binary operators *)
type binop =
  | Add      (* + *)
  | Sub      (* - *)
  | Mult     (* * *)
  | Div      (* / *)
  | Mod      (* % *)
  | Pow      (* ** *)
  | Eq       (* == *)
  | Neq      (* != *)
  | Less     (* < *)
  | Leq      (* <= *)
  | Greater  (* > *)
  | Geq      (* >= *)

(* Expressions *)
type expr =
  | Int_lit of int                (* 42 *)
  | Float_lit of float            (* 42.0 *)
  | String_lit of string          (* "Hello, world" *)
  | Bool_lit of bool              (* true *)
  | Unop of unop * expr           (* -5 *)
  | Binop of expr * binop * expr  (* a + b *)
  | Id of string                  (* x *)
  | Assign of string * expr       (* x = 4 *)
  | Call of expr * expr list      (* add(1, 2) *)
  | List of expr list             (* [1, 2, 3] *)
  | Fdecl of fdecl                (* (x) -> ... return x *)


(* Function Declarations *)
and fdecl =
  {
    params: expr list;    (* Parameters *)
    body: stmt list;      (* Function Body *)
    return: expr          (* Return *)
  }
  
(* Statements *)
and stmt =
  | Do of expr     (* set foo = bar + 3 *)

(* Program entry point *)
type program = stmt list
