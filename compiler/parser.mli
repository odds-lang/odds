type token =
  | LPAREN
  | RPAREN
  | LCAR
  | RCAR
  | LBRACE
  | RBRACE
  | COMMA
  | VBAR
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | POWER
  | EQ
  | NEQ
  | LEQ
  | GEQ
  | AND
  | OR
  | NOT
  | ASN
  | IF
  | THEN
  | ELSE
  | DO
  | FDELIM
  | RETURN
  | CAKE
  | EOF
  | ID of (string)
  | NUM_LITERAL of (Ast.num)
  | STRING_LITERAL of (string)
  | BOOL_LITERAL of (bool)
  | VOID_LITERAL

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
