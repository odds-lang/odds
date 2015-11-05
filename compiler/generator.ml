(*
 * COMS4115: Odds parser
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Ast
open Printf

let func_to_text f arg = match f with
  | "print" -> sprintf "print(\"%s\")" arg
  | _ -> "" (* f(args) *)

let op_to_text op = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"
