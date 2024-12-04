open Ast

let (memory:str*exprval) = []  

let parse (s : string)  =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
(*BIG STEP*)
let rec eval_expr st e= 
  match e with
    True -> Bool(true)
  | False -> Bool(false)
  | Var(v) -> st e
  | Const(n) -> Nat(n)
  | Not(e) ->( 
      match eval_expr e with
      | Bool(b) -> Bool(not b)
      | _ -> raise (TypeError "not should be on a bool")
  )
  | And (e1,e2) -> (
      match (eval_expr e1,eval_expr e2) with
      | Bool(b1),Bool(b2) -> Bool(b1 && b2) 
      | _ -> raise (TypeError "and should be on two bools")
  )
  | Or (e1,e2) -> (
      match (eval_expr e1,eval_expr e2) with
      | Bool(b1),Bool(b2) -> Bool(b1 || b2) 
      | _ -> raise (TypeError "or should be on two bools")
  )
  | Add (e1,e2) -> (
      match (eval_expr e1,eval_expr e2) with
      | Nat(n1),Nat(n2) -> Nat(n1 + n2) 
      | _ -> raise (TypeError "add should be on two nats")
  )
  | Sub (e1,e2) -> (
      match (eval_expr e1,eval_expr e2) with
      | Nat(n1),Nat(n2) -> Nat(n1 - n2) 
      | _ -> raise (TypeError "sub should be on two nats")
  )
  | Mul (e1,e2) -> (
      match (eval_expr e1,eval_expr e2) with
      | Nat(n1),Nat(n2) -> Nat(n1 * n2) 
      | _ -> raise (TypeError "mul should be on two nats")
  )
  | Eq (e1,e2) -> (
      match (eval_expr e1,eval_expr e2) with
      | Nat(n1),Nat(n2) -> Bool(n1 = n2) 
      | _ -> raise (TypeError "eq should be on two nats")
  )
  | Leq (e1,e2) -> (
      match (eval_expr e1,eval_expr e2) with
      | Nat(n1),Nat(n2) -> Bool(n1 <= n2) 
      | _ -> raise (TypeError "leq should be on two nats")
  )

(*SMALL STEP*)

let rec trace1 = function
  (*SKIP*)
  | Cmd(st,Skip)-> St(st)
  | Cmd(st,Assign(x,e)) ->( 
      let v = eval_expr st e in 
      let st x = v in  
      St(st))
  (*SEQUENCE*)
  | Cmd(st,Seq(c1,c2)) ->(
      let Cmd(st',c1') = trace1 Cmd(st,c1) in
      match trace1 Cmd(st,c1) with
      | Cmd(st',c1') -> Cmd(st',Seq(c1',c2))
      | St(st') -> Cmd(st',c2))
  (*IF*)
  | Cmd(st,If(e,c1,c2)) ->(
      match eval_expr st e with
      | Bool(False) -> Cmd(st,c2) 
      | Bool(True) -> Cmd(st,c1) 
      | _ -> raise (TypeError "if condition should be a bool"))
  (*WHILE*)
  | Cmd(st,While(e,c)) -> (
      match eval_expr st e with
      |Bool(False) -> St(st)
      |Bool(True) -> Cmd(st,Seq(c,While(e,c))))
  (*EXCEPTION*)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


