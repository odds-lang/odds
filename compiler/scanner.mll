(*
 * Odds scanner
 *
 * Authors: Alex Kalicki, Alexandra Medway, Daniel Echikson, Lilly Wang
 * COMS4115
 *)

{ open Parser }

let num = ['0'-'9']

rule token = parse
  
(* Whitespace *)
| [' ' '\n' '\r'] { token lexbuf }

(* Comments *)
| "/*"		{ comment lexbuf }

(* Punctuation *)
| '('		{ LPAREN }  | ')'		{ RPAREN }
| '<'		{ LCAR }	| '>'		{ RCAR } (* Also relational operators *)
| '['		{ LBRACK }	| ']'		{ RBRACK }
| ';'		{ SEMI }    | ':'		{ COLON }
| ','		{ COMMA }	| '|'		{ VBAR }

(* Arithmetic Operators *)
| '+'		{ PLUS }    | '-'		{ MINUS }
| '*'		{ TIMES }   | '/'		{ DIVIDE }
| '%'		{ MOD }

(* Relational Operators *)
| "=="		{ EQ }		| "!="		{ NEQ }
| "<="		{ LEQ }		| ">="		{ GEQ }

(* Logical Operators & Keywords*)
| "&&"		{ AND }		| "||"		{ OR }
| "!"		{ NOT }

(* Assignment Operator *)
| '='		{ ASN }

(* Conditional Operators *)
| "if"		{ IF }		| "then"	{ THEN }
| "else"	{ ELSE }

(* Declarative Keywords *)
| "set"		{ SET }		| "state"	{ STATE }

(* Function Symbols & Keywords *)
| "->"		{ FDELIM }	| "=>"		{ FRTYPE }
| "return"	{ RETURN }

(* Type Keywords *)
| "int"		{ INT }		| "float"	{ FLOAT }
| "string"	{ STRING }	| "dist"	{ DIST }
| "list"	{ LIST }	| "void"	{ VOID }
| "bool"	{ BOOL }

(* End-of-File *)
| eof { EOF }

(* Identifiers *)
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_']* as lxm { ID(lxm) }

(* Literals *)
| '-'?num+ as intlit { INT_LITERAL(int_of_string intlit) }
| num* ['.'] num+ as floatlit { FLOAT_LITERAL(float_of_string floatlit) }
| '"' ( ([^ '"'] | "\"")* as strlit) '"' { STRING_LITERAL(strlit) }
| "true" | "false" as boollit { BOOL_LITERAL(bool_of_string boollit)}
(* To Do - List Literals - Do they even go here? *)

and comment = parse
| "*/"		{ token lexbuf }
| _			{ comment lexbuf }
