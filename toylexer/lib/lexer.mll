{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let capital_letter = ['A'-'Z']
let atok = capital_letter chr*
let lcvowel = ['a' 'e' 'i' 'o' 'u']
let vowel = ['A' 'E' 'I' 'O' 'U'] |lcvowel 
let not_vowel = letter # vowel
let btok = lcvowel*
let ctok = not_vowel* vowel? not_vowel*
let dtok = ['-']? num* ['.']? num*
let etok = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | etok {ETOK}
  | dtok {DTOK}
  | atok {ATOK}
  | btok {BTOK}
  | ctok {CTOK}
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
