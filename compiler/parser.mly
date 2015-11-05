(*
 * COMS4115: Odds parser
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

%{ open Ast %}

%token STATE EOF
%token LPAREN RPAREN PLUS MINUS TIMES DIVIDE MOD POWER
%token <int> INT_LITERAL
%token <float> FLOAT_LIT

/* Precedence and associativity of each operator */
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left POWER

%start program                /* Start symbol */
%type <Ast.program> program   /* Type returned by a program */

%%

program:
  | stmt_list EOF    { $1 }

stmt_list:
  | /* nothing */    { [] }
  | stmt_list stmt   { $2 :: $1 }

stmt:
  | STATE expr    { Expr($1) }

expr:
  | INT_LITERAL           { Literal($1) }
  | FLOAT_LIT             { Literal($1) }
  | expr PLUS expr        { Binop($1, Add, $3) }
  | expr MINUS expr       { Binop($1, Sub, $3) }
  | expr TIMES expr       { Binop($1, Mult, $3) }
  | expr DIVIDE expr      { Binop($1, Div, $3) }
  | expr MOD expr         { Binop($1, Mod, $3) }
  | expr POWER expr       { Binop($1, Pow, $3) }
  | LPAREN expr RPAREN    { $2 }
