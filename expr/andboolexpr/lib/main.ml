open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | Not (e) -> "Not(" ^ (string_of_boolexpr e)
  | And (e0,e1) -> "And("^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | Or (e0,e1) -> "And("^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"


let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> Not(trace1 e)
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> And(trace1 e1, e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> Or(trace1 e1, e2)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
    True -> true
  | False -> false
  | Not(e) -> not (eval e)
  | And (e1,e2) -> (eval e1) && (eval e2)
  | Or (e1,e2) -> (eval e1) || (eval e2)
  | If(c,t,f) -> if eval c then eval t else eval f
