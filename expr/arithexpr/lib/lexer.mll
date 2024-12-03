{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "or" { OR }
  | "and" { AND }
  | "not" { NOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "0" { ZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | "iszero" { ISZERO }
  | eof { EOF }
