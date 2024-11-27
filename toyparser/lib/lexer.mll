{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex = '0'['x''X']['0'-'9''a'-'f''A'-'F']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIVIDE }
  | hex { CONST (Lexing.lexeme lexbuf)}
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
