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
%token LPAREN RPAREN LCAR RCAR LBRACE RBRACE LBRACK RBRACK COMMA VBAR

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
%token DO

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
%right ASN
%nonassoc RETURN
%left EQ NEQ
%left LCAR LEQ RCAR GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left POWER
%nonassoc NOT

%start program                /* Start symbol */
%type <Ast.program> program   /* Type returned by a program */

%%

/* declarations */
stmt_list:
  | /* nothing */               { [] }
  | stmt_list stmt              { $2 :: $1 }

stmt:
  | DO expr                     { Do($2) }

fparams_list:
  | ID                          { [Id($1)] }
  | fparams_list COMMA ID       { Id($3)::$1 }

fparams:
  | /* nothing */               { [] }
  | fparams_list                { List.rev $1 }

fdecl:
  | LBRACK fparams RBRACK FDELIM stmt_list RETURN expr
    { {
      params = $2;
      body = $5;
      return = $7;
    } }

program:
  | stmt_list EOF               { $1 }

args_opt:
  | /* nothing */               { [] }
  | args_list                   { List.rev $1 }

args_list:
  | expr                        { [$1] }
  | args_list COMMA expr        { $3 :: $1 }

/* expressions */
literal:
  | INT_LITERAL                 { Int_lit($1) }
  | FLOAT_LITERAL               { Float_lit($1) }
  | STRING_LITERAL              { String_lit($1) }

expr:
  | literal                     { $1 }
  | arith                       { $1 }
  | boolean                     { $1 }
  | ID                          { Id($1) }
  | ID LPAREN args_opt RPAREN   { Call($1, $3)}
  | LPAREN expr RPAREN          { $2 }
  | fdecl                       { Fdecl($1) }
  | ID ASN expr                 { Assign($1, $3) }

arith:
  | MINUS expr                  { Unop(Sub, $2) }
  | expr PLUS expr              { Binop($1, Add, $3) }
  | expr MINUS expr             { Binop($1, Sub, $3) }
  | expr TIMES expr             { Binop($1, Mult, $3) }
  | expr DIVIDE expr            { Binop($1, Div, $3) }
  | expr MOD expr               { Binop($1, Mod, $3) }
  | expr POWER expr             { Binop($1, Pow, $3) }

boolean:
  | NOT expr                    { Unop(Not, $2) }
  | expr EQ expr                { Binop($1, Eq, $3) }
  | expr NEQ expr               { Binop($1, Neq, $3) }
  | expr LCAR expr              { Binop($1, Less, $3) }
  | expr LEQ expr               { Binop($1, Leq, $3) }
  | expr RCAR expr              { Binop($1, Greater, $3) }
  | expr GEQ expr               { Binop($1, Geq, $3) }

