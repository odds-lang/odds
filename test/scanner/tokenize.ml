(* equivalent to parser.mli when we write that *)
open Tokens

let stringify = function
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVIDE -> "DIVIDE"
  | ASSIGN -> "ASSIGN"
  | SEQUENCER -> "SEQUENCER"
  | LITERAL(int) -> "LITERAL"
  | VARIABLE(int) -> "VARIABLE"
  | EOF -> "EOF"

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)


let stringify = function
(* Punctuation *)
| LPAREN ->  | RPAREN ->
| LCAR ->    | RCAR ->
| LBRACK ->  | RBRACK ->
| SEMI ->    | COLON ->
| COMMA ->   | VBAR ->

(* Arithmetic Operators *)
| PLUS ->    | MINUS ->
| TIMES ->   | DIVIDE ->
| MOD ->

(* Relational Operators *)
| EQ ->    | NEQ ->
| LEQ ->   | GEQ ->

(* Logical Operators & Keywords*)
| AND ->   | OR ->
| NOT ->

(* Assignment Operator *)
| ASN ->

(* Conditional Operators *)
| IF ->    | THEN ->
| ELSE ->

(* Declarative Keywords *)
| SET ->   | STATE ->

(* Function Symbols & Keywords *)
| FDELIM ->  (*| FRTYPE *)
| RETURN ->

(* End-of-File *)
| EOF ->

(* Identifiers *)
| ID of (string) ->

(* Literals *)
| INT_LITERAL of (int) ->
| FLOAT_LITERAL of (float) ->
| STRING_LITERAL of (string) ->
| BOOL_LITERAL of (bool)
