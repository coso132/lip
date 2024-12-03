%{
open Ast
%}

%token TRUE
%token FALSE
%token OR
%token AND
%token NOT 
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token EOF

%start <expr> prog

%nonassoc IF THEN ELSE
%nonassoc OR
%nonassoc AND
%nonassoc NOT
%nonassoc ISZERO
%nonassoc SUCC PRED 

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e1 = expr { Not (e1)}
  | e1 = expr; AND; e2 = expr { And (e1,e2) }
  | e1 = expr; OR; e2 = expr { Or (e1,e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | ZERO { Zero}
  | SUCC; e=expr { Succ(e)}
  | PRED; e=expr { Pred(e)}
  | ISZERO; e=expr { IsZero(e)}
  | LPAREN; e=expr; RPAREN {e}
;

