{
open Parser
}

let white = [' ' '\t' '\n']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let varname = ['a'-'z']+

(*
 x, var, := , = ,<=, while, do, ;
 *)
rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "var" { VAR }
  | ":=" { ASSIGN  }
  | "<=" { LEQ }
  | "=" { EQ }
  | "-" { MINUS }
  | "+" { PLUS }
  | "*" { TIMES }
  | "or" { OR }
  | "and" { AND }
  | "not" { NOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | ";" { SEQ }
  | "skip" {SKIP}
  | varname { VARNAME(Lexing.lexeme lexbuf)}
  | num { CONST(Lexing.lexeme lexbuf)}
  | eof { EOF }
