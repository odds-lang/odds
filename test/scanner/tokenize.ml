(* equivalent to parser.mli when we write that *)
open Tokens

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
| LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
| LCAR -> "LCAR"      | RCAR -> "RCAR"
| LBRACK -> "LBRACK"  | RBRACK -> "RBRACK"
| SEMI -> "SEMI"      | COLON -> "COLON"
| COMMA -> "COMMA"    | VBAR -> "VBAR"

(* Arithmetic Operators *)
| PLUS -> "PLUS"     | MINUS -> "MINUS"
| TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"
| MOD -> "MOD"

(* Relational Operators *)
| EQ -> "EQ"    | NEQ -> "NEQ"
| LEQ -> "LEQ"  | GEQ -> "GEQ"

(* Logical Operators & Keywords*)
| AND -> "AND"   | OR -> "OR"
| NOT -> "NOT"

(* Assignment Operator *)
| ASN -> "ASN"

(* Conditional Operators *)
| IF -> "IF"    | THEN -> "THEN"
| ELSE -> "ELSE"

(* Declarative Keywords *)
| SET -> "SET"   | STATE -> "STATE"

(* Function Symbols & Keywords *)
| FDELIM -> "FDELIM"  (*| FRTYPE *)
| RETURN -> "RETURN"

(* End-of-File *)
| EOF -> "EOF"

(* Identifiers *)
| ID of (string) -> "ID"

(* Literals *)
| INT_LITERAL of (int) -> "INT_LITERAL"
| FLOAT_LITERAL of (float) -> "FLOAT_LITERAL"
| STRING_LITERAL of (string) -> "STRING_LITERAL"
| BOOL_LITERAL of (bool) -> "BOOL_LITERAL"
