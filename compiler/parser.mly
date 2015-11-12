/*
 * COMS4115: Odds parser
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 */

%{ open Ast %}

/* Punctuation */
%token LPAREN RPAREN LCAR RCAR LBRACK RBRACK SEMI COMMA VBAR

/* Arithmetic Operators */
%token PLUS MINUS TIMES DIVIDE MOD POWER

/* Relational Operators */
%token EQ NEQ LEQ GEQ

/* Logical Operators & Keywords*/
%token AND OR NOT

/* Assignment Operator */
%token ASN

/* Conditional Operators */
%token IF THEN ELSE

/* Declarative Keywords */
%token SET STATE

/* Function Symbols & Keywords */
%token FDELIM RETURN

/* End-of-File */
%token EOF

/* Identifiers */
%token <string> ID

/* Literals */
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token VOID_LITERAL

/* Precedence and associativity of each operator */
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left POWER

%start program                /* Start symbol */
%type <Ast.program> program   /* Type returned by a program */

%%

program:
  | stmt_list EOF               { $1 }

args_opt:
  | /* nothing */               { [] }
  | args_list                   { List.rev $1 }

args_list:
  | expr                        { [$1] }
  | args_list COMMA expr        { $3 :: $1 }
 
(* expressions *)
literal:
  | INT_LITERAL                 { Int_lit($1) }
  | FLOAT_LITERAL               { Float_lit($1) }
  | STRING_LITERAL              { String_lit($1) }
  
expr:
  | literal                     { $1 }
  | ID LPAREN args_opt RPAREN   { Call($1, $3)}
  | ID                          { Id($1) }
  | MINUS expr                  { Unop(Sub, $2) }
  | expr PLUS expr              { Binop($1, Add, $3) }
  | expr MINUS expr             { Binop($1, Sub, $3) }
  | expr TIMES expr             { Binop($1, Mult, $3) }
  | expr DIVIDE expr            { Binop($1, Div, $3) }
  | expr MOD expr               { Binop($1, Mod, $3) }
  | expr POWER expr             { Binop($1, Pow, $3) }
  | LPAREN expr RPAREN          { $2 }

(* declarations *)
stmt_list:
  | /* nothing */               { [] }
  | stmt_list stmt              { $2 :: $1 }

stmt:
  | STATE expr                  { State($2) }
  | SET ID ASN expr             { Set($2, $4) }

fdecl:

