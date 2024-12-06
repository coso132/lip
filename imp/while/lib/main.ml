open Ast
open Types

let parse (s : string)  =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(*BIG STEP*)

let rec eval_expr st e = 
  match e with
  |  True -> Bool(true)
  | False -> Bool(false)
  | Var(v) -> st v
  | Const(n) -> Nat(n)
  | Not(e) ->( 
      match eval_expr st e with
      | Bool(b) -> Bool(not b)
      | _ -> raise (TypeError "not should be on a bool"))
  | And (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Bool(b1),Bool(b2) -> Bool(b1 && b2) 
      | _ -> raise (TypeError "and should be on two bools"))
  | Or (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Bool(b1),Bool(b2) -> Bool(b1 || b2) 
      | _ -> raise (TypeError "or should be on two bools"))
  | Add (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Nat(n1),Nat(n2) -> Nat(n1 + n2) 
      | _ -> raise (TypeError "add should be on two nats"))
  | Sub (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Nat(n1),Nat(n2) -> Nat(n1 - n2) 
      | _ -> raise (TypeError "sub should be on two nats"))
  | Mul (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Nat(n1),Nat(n2) -> Nat(n1 * n2) 
      | _ -> raise (TypeError "mul should be on two nats"))
  | Eq (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Nat(n1),Nat(n2) -> Bool(n1 = n2) 
      | _ -> raise (TypeError "eq should be on two nats"))
  | Leq (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Nat(n1),Nat(n2) -> Bool(n1 <= n2) 
      | _ -> raise (TypeError "leq should be on two nats"))

(*SMALL STEP*)

let bot = fun x -> raise (UnboundVar x)
let bind st x v = fun y -> if (String.compare x y )=0 then v else st y 

let rec trace1 c = 
  match c with 
  | St(_) -> (raise NoRuleApplies)
  | Cmd(cmd,st) -> 
    let (<--) x e = bind st x (eval_expr st e) in 
    match Cmd(cmd,st) with 
    (*SKIP*)
    | Cmd(Skip,st)-> St(st)
    (*ASSIGN*)
    | Cmd(Assign(x,e),_) ->( 
        St(x <-- e ))
    (*SEQUENCE*)
    | Cmd(Seq(c1,c2),st) ->(
        match trace1 (Cmd(c1,st)) with
        | St(st') -> Cmd(c2,st')
        | Cmd(c1',st') -> Cmd(Seq(c1',c2),st'))
    (*IF*)
    | Cmd(If(e,c1,c2),st) ->(
        match eval_expr st e with
        | Bool(false) -> Cmd(c2,st) 
        | Bool(true) -> Cmd(c1,st) 
        | _ -> raise (TypeError "if condition should be a bool"))
    (*WHILE*)
    | Cmd(While(e,c),st) -> (
        match eval_expr st e with
        | Bool(false) -> St(st)
        | Bool(true) -> Cmd(Seq(c,While(e,c)),st)
        | _ -> raise (TypeError "while condition should be a bool"))
    (*EXCEPTION*)
    | St(_) -> (raise NoRuleApplies)

let rec trace_rec n t = 
  if n<=0 then [t]
  else try
    let t' = trace1 t
    in t::(trace_rec (n-1) t')
  with NoRuleApplies -> [t]

let trace n t = trace_rec n (Cmd(t,bot))

