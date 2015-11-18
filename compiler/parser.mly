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
%token LPAREN RPAREN LCAR RCAR LBRACE RBRACE COMMA VBAR

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
%token <Ast.num> NUM_LITERAL
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
%right NOT

%start program                /* Start symbol */
%type <Ast.program> program   /* Type returned by a program */

%%

/* Program flow */
program:
  | stmt_list EOF               { List.rev $1 }

stmt_list:
  | /* nothing */               { [] }
  | stmt_list stmt              { $2 :: $1 }

stmt:
  | DO expr                     { Do($2) }

/* Expressions */
expr:
  | literal                     { $1 }
  | arith                       { $1 }
  | boolean                     { $1 }
  | ID                          { Id($1) }
  | ID ASN expr                 { Assign($1, $3) }
  | ID LPAREN list_opt RPAREN   { Call(Id($1), $3) }
  | LBRACE list_opt RBRACE      { List($2) }
  | LPAREN expr RPAREN          { $2 }
  | fdecl                       { Fdecl($1) }

/* Function declaration */
fdecl:
  | LPAREN fparams_opt FDELIM stmt_list RETURN expr
    { {
      params = $2;
      body = List.rev $4;
      return = $6;
    } }

fparams_opt:
  | /* nothing */               { [] }
  | fparam_list                 { List.rev $1 }

fparam_list:
  | ID                          { [Id($1)] }
  | fparam_list COMMA ID        { Id($3)::$1 }

/* Function calling */
list_opt:
  | /* nothing */               { [] }
  | list                        { List.rev $1 }

list:
  | expr                        { [$1] }
  | list COMMA expr             { $3 :: $1 }

/* Binary operators */
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

/* Literals */
literal:
  | NUM_LITERAL                 { Num_lit($1) }
  | STRING_LITERAL              { String_lit($1) }
  | BOOL_LITERAL                { Bool_lit($1) }
