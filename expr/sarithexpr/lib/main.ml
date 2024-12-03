open Ast

type exprval = Bool of bool | Nat of int
type exprtype = BoolT | NatT

let string_of_type = function
  | BoolT -> "BoolT"
  | NatT -> "NatT"

let string_of_val = function
  | Nat(n) -> string_of_int(n)
  |Bool(b) -> string_of_bool(b)

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Not (e) -> "Not(" ^ (string_of_expr e)
  | And (e0,e1) -> "And("^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Or (e0,e1) -> "And("^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) ->"Succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) ->"Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) ->"IsZero(" ^ (string_of_expr e) ^ ")"


let parse (s : string)  =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec is_nv = function
  | Zero -> true
  | Succ(n) -> is_nv n
  | _ -> false


(*Type checking*)
exception TypeError of string 
let err_info e found expected = 
   (
    string_of_expr e ^ " has type " ^ string_of_type found ^ 
    ", but type "^ string_of_type expected ^ " was expected" 
    ) 

let huh e expected return typec= let e' = typec e in 
      match e' with
      | n when n = expected -> return
      | _ -> raise (TypeError (err_info e e' expected ) )
 
let rec typecheck = function
  | True |False-> BoolT
  | Zero -> NatT
(*
  | Succ(e) -> huh e NatT NatT typecheck
  | Pred(e) when is_nv (Pred(e))-> huh e NatT NatT typecheck
  | Pred(e) -> raise (TypeError (string_of_expr e ^ " is not a natural number"))
  *)
  | Succ(e) -> huh e NatT NatT typecheck 
  | Pred(e) when is_nv (Pred(e))-> NatT
  | Pred(e) -> raise (TypeError (string_of_expr e ^ " is not a natural number"))
  | IsZero(e) -> huh e NatT BoolT typecheck
  | Not(e) -> huh e BoolT BoolT typecheck
  | And(e1,e2) | Or(e1,e2) ->(
      let _ = huh e1 BoolT BoolT typecheck in
      huh e2 BoolT BoolT typecheck
  )
  | If(e1,e2,e3) ->(
    let _ = huh e1 BoolT BoolT typecheck in
    let tt = typecheck e2 in
    huh e3 tt tt typecheck 
  )

(*SMALL STEP*)

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
  | Succ(e) -> Succ(trace1 e)
  | Pred(Zero) -> raise NoRuleApplies 
  | Pred(Succ(nv)) when is_nv nv -> nv
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True 
  | IsZero(Succ(nv)) when is_nv nv -> False 
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

(*BIG STEP*)

let rec eval = function
    True -> Bool(true)
  | False -> Bool(false)
  | Not(e) ->( 
      match eval e with
      | Bool(b) -> Bool(not b)
      | _ -> raise (TypeError "not should be on a bool")
  )
  | And (e1,e2) -> (
      match (eval e1,eval e2) with
      | Bool(b1),Bool(b2) -> Bool(b1 && b2) 
      | _ -> raise (TypeError "and should be on two bools")
  )
  | Or (e1,e2) -> (
      match (eval e1,eval e2) with
      | Bool(b1),Bool(b2) -> Bool(b1 || b2) 
      | _ -> raise (TypeError "or should be on two bools")
  )
  | If(e1,e2,e3) -> (
      match (eval e1 ,eval e2 ,eval e3) with
      | (Bool(c),Nat(t),Nat(f)) -> if c then Nat(t) else Nat(f)
      | (Bool(c),Bool(t),Bool(f)) -> if c then Bool(t) else Bool(f)
      | _ -> raise (TypeError "if conditon should be a bool, and within then and else the same type")
  )
  | Zero -> Nat(0)
  | Succ(e) ->( 
      match (eval e) with 
      | Nat(n) -> Nat(n+1)
      | _ -> raise (TypeError "succ should apply to nat numbers")
  )
  | Pred(e) ->( 
      match (eval e) with 
      | Nat(n) when n>0 -> Nat(n-1)
      | _ -> raise (TypeError "pred should apply to nat numbers")
  )
  | IsZero(e) ->(
      match (eval e) with 
      | Nat(n) ->Bool(n=0) 
      | _ -> raise (TypeError "iszero should apply to nat numbers")
  )

