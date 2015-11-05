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
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL

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

literal:
  | INT_LITERAL           { Int_Lit($1) }
  | FLOAT_LITERAL         { Float_lit($1) }
  | STRING_LITERAL        { String_lit($1) }

expr:
  | literal
  | MINUS expr            { Unop(Sub, $2) }
  | expr PLUS expr        { Binop($1, Add, $3) }
  | expr MINUS expr       { Binop($1, Sub, $3) }
  | expr TIMES expr       { Binop($1, Mult, $3) }
  | expr DIVIDE expr      { Binop($1, Div, $3) }
  | expr MOD expr         { Binop($1, Mod, $3) }
  | expr POWER expr       { Binop($1, Pow, $3) }
  | LPAREN expr RPAREN    { $2 }
