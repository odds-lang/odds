(*
 * COMS4115: Odds Syntactically Checked abstract syntax tree
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
  | Int_lit of int               (* 42 *)
  | Float_lit of float           (* 42.0 *)
  | String_lit of string         (* "Hello, world" *)
  | Bool_lit of bool             (* true *)
  | Unop of op * expr            (* -5 *)
  | Binop of expr * op * expr    (* a + b *)
  | Id of string                 (* x *)
  | Assign of string * expr      (* x = 4 *)
  | Call of expr * expr list     (* add(1, 2) *)
  | List of expr list            (* [1, 2, 3] *)
  | Fdecl of fdecl               (* (x) -> ... return x *)


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






type sdata_type = 
  | Num
  | String
  | Bool
  | List
  | Unrestrained           (* For Ambiguous/unrestrained *)

type sexpr = 
  | Num_lit of Ast.num
  | String_lit of string
  | Bool_lit of bool
  | Unop of Ast.unop * sexpr
  | Binop of sexpr * Ast.binop * sexpr
  | Id of string
  | Assign of string * sexpr
  | Call of sexpr * sexpr list
  | List of sexpr list

and sstmt = Do of sexpr

and sprogram = sstmt list

and svar = 
  {
    sname: string;
    sstatic_scoping_name: string;
    stype: sdata_type;
  }

and sfunc_decl = 
  {
    sname: string;
    sstatic_scoping_name: string;
    sparams: sid list;
    sbody: sstmt list;
    sreturn: sexpr;
    sreturn_type: sdata_type;
    is_anonymous: bool;
  }

type program = stmt list
