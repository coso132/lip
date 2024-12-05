%{
open Ast
%}

%token TRUE
%token FALSE

%token ASSIGN

%token <string> CONST
%token <string> VARNAME

%token LEQ
%token EQ

%token MINUS
%token PLUS
%token TIMES

%token OR
%token AND
%token NOT 

%token LPAREN
%token RPAREN

%token IF
%token THEN
%token ELSE

%token WHILE
%token DO

%token SEQ
%token SKIP
%token EOF

%left SEQ 
%nonassoc ELSE 
%left OR 
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left TIMES


%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | v = VARNAME { Var(v)}
  | n = CONST { Const(int_of_string n)}
  | NOT; e1 = expr { Not (e1)}
  | e1 = expr; AND; e2 = expr { And (e1,e2) }
  | e1 = expr; OR; e2 = expr { Or (e1,e2) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; TIMES; e2 = expr { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;

cmd:
  | SKIP                                      { Skip }
  | v=VARNAME; ASSIGN; e=expr                 { Assign(v,e) }
  | c1=cmd; SEQ; c2=cmd                       { Seq(c1,c2) }
  | IF; e=expr; THEN; c1=cmd; ELSE; c2=cmd    { If(e,c1,c2) } 
  | WHILE; e=expr; DO; LPAREN; c=cmd; RPAREN { While(e,c) } 
  | LPAREN; c=cmd; RPAREN                     { c }
