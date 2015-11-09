open Parser

let stringify = function
  (* Punctuation *)
  | LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
  | LCAR -> "LCAR"      | RCAR -> "RCAR"
  | LBRACK -> "LBRACK"  | RBRACK -> "RBRACK"
  | SEMI -> "SEMI"
  | COMMA -> "COMMA"    | VBAR -> "VBAR"

  (* Sequencing *)
  | SEQ -> "SEQ"

  (* Arithmetic Operators *)
  | PLUS -> "PLUS"     | MINUS -> "MINUS"
  | TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"
  | MOD -> "MOD"       | POWER -> "POWER" 

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

  (* Function Symbols & Keywords *)
  | FDELIM -> "FDELIM"
  | RETURN -> "RETURN"

  (* End-of-File *)
  | EOF -> "EOF"

  (* Identifiers *)
  | ID(string) -> "ID"

  (* Literals *)
  | INT_LITERAL(int) -> "INT_LITERAL"
  | FLOAT_LITERAL(float) -> "FLOAT_LITERAL"
  | STRING_LITERAL(string) -> "STRING_LITERAL"
  | BOOL_LITERAL(bool) -> "BOOL_LITERAL"
  | VOID_LITERAL -> "VOID_LITERAL"

let _ = 
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
