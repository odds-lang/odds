(*
 * COMS4115: Odds scanner
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

{ 
  open Ast
  open Parser

  exception Illegal_Character of string
}

let numeric = ['0'-'9']
let whitespace = [' ' '\n' '\r']

rule token = parse

(* Whitespace *)
| whitespace*    { token lexbuf }

(* Comments *)
| "/*"    { comment lexbuf }

(* Function Symbols & Keywords *)
| ')' whitespace* "->"   { FDELIM }  | "return"   { RETURN }

(* Punctuation *)
| '('   { LPAREN }  | ')'   { RPAREN }
| '<'   { LCAR }    | '>'   { RCAR } (* Also relational operators *)
| '['   { LBRACE }  | ']'   { RBRACE }
| ','   { COMMA }   | '|'   { VBAR }

(* Arithmetic Operators *)
| '+'   { PLUS }    | '-'   { MINUS }
| '*'   { TIMES }   | '/'   { DIVIDE }
| '%'   { MOD }     | "**"  { POWER }

(* Relational Operators *)
| "=="    { EQ }    | "!="    { NEQ }
| "<="    { LEQ }   | ">="    { GEQ }

(* Logical Operators & Keywords*)
| "&&"    { AND }   | "||"    { OR }
| "!"     { NOT }

(* Assignment Operator *)
| '='   { ASN }

(* Conditional Operators *)
| "if"    { IF }    | "then"  { THEN }
| "else"  { ELSE }

(* Declarative Keywords *)
| "do"    { DO }

(* Literals *)
| numeric+ as intlit { NUM_LITERAL(Ast.Num_int(int_of_string intlit)) }
| numeric* '.' numeric+ as floatlit 
    { NUM_LITERAL(Ast.Num_float(float_of_string floatlit)) }
| '"' (([^ '"'] | "\\\"")* as strlit) '"' { STRING_LITERAL(strlit) }
| "true" | "false" as boollit { BOOL_LITERAL(bool_of_string boollit)}
| "void" { VOID_LITERAL }

(* Identifiers *)
| ['a'-'z' 'A'-'Z' '_'] (['a'-'z' 'A'-'Z' '_' ] | numeric)* as lxm { ID(lxm) }

(* Invalid Token *)
| _ as char { 
    let message = "Illegal Character '" ^ Char.escaped char ^ "'" in
    raise (Illegal_Character message)
  }

(* End-of-File *)
| eof { EOF }
| _ as c { failwith("illegal character " ^ Char.escaped c) }

and comment = parse
| "*/"    { token lexbuf }
| _       { comment lexbuf }
