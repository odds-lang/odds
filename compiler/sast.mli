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