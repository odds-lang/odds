(*
 * Odds scanner
 *
 * Authors: Alex Kalicki, Alexandra Medway, Daniel Echikson, Lilly Wang
 * COMS4115
 *)

{ open Parser }

let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_']*
let int = ['0'-'9']+
let float = ['0'-'9']* ['.'] ['0'-'9']+


rule token = parse
  
 (* Whitespace *)
| [' ' '\n' '\r'] { token lexbuf }

