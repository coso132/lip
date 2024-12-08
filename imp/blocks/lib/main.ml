open Ast
open Types

let parse (s : string)  =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(*BIG STEP*)

let find (st:state) (x:ide) : memval=
  let (_,mem,_) = st in
  let env =  topenv st in
  match env x with
  | None -> raise (UnboundVar x)
  | Some(IVar(loc))| Some(BVar(loc)) ->
  match mem loc with 
  | None -> failwith "memory error"
  | Some(memval) -> memval
   

let rec eval_expr (st:state) (e:expr) :memval = 
  match e with
  | True -> Bool(true)
  | False -> Bool(false)
  | Var(v) -> find st v
  | Const(n) -> Int(n)
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
      | Int(n1),Int(n2) -> Int(n1 + n2) 
      | _ -> raise (TypeError "add should be on two nats"))
  | Sub (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Int(n1),Int(n2) -> Int(n1 - n2) 
      | _ -> raise (TypeError "sub should be on two nats"))
  | Mul (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Int(n1),Int(n2) -> Int(n1 * n2) 
      | _ -> raise (TypeError "mul should be on two nats"))
  | Eq (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Int(n1),Int(n2) -> Bool(n1 = n2) 
      | _ -> raise (TypeError "eq should be on two nats"))
  | Leq (e1,e2) -> (
      match (eval_expr st e1,eval_expr st e2) with
      | Int(n1),Int(n2) -> Bool(n1 <= n2) 
      | _ -> raise (TypeError "leq should be on two nats"))

(*let bot = fun x -> raise (UnboundVar x)*)
(*let bind st x v = fun y -> if x=y then Some(v) else st y *)
let bot = fun _ -> None 

let bind_mem mem loc memval = 
  fun value -> if value = loc then Some(memval) else mem value 

let bind_env env x loc = 
  fun y -> if y = x then Some(loc) else env y 

(*SMALL STEP*)
let rec trace1 c = 
  match c with 
  | St(_) -> raise (NoRuleApplies)
  | Cmd(cmd,st) -> 
  let (_,mem,loc) = st in 
  let envl = getenv st in
  let env = topenv st in
  let tenv = popenv st in 
  let (<-<) l e = bind_mem mem l (eval_expr st e) in 
  match cmd with 
  | Skip-> St(st)
  | Assign(x,e) ->( 
      let mem' = match env x with
      | None -> raise (UnboundVar x)
      | Some(IVar(loc))|Some(BVar(loc)) -> loc <-< e
      in St(env::tenv,mem',loc)
  )
  | Seq(c1,c2) ->(
    match trace1 (Cmd(c1,st)) with
      | St(st') -> Cmd(c2,st')
      | Cmd(c1',st') -> Cmd(Seq(c1',c2),st')
  )
  | If(e,c1,c2) ->(
      match eval_expr st e with
      | Bool(false) -> Cmd(c2,st) 
      | Bool(true) -> Cmd(c1,st) 
      | _ -> raise (TypeError "if condition should be a bool")
  )
  | While(e,c) -> (
      match eval_expr st e with
      | Bool(false) -> St(st)
      | Bool(true) -> Cmd(Seq(c,While(e,c)),st)
      | _ -> raise (TypeError "while condition should be a bool")
  )
  | Decl(d,c) ->   
      let rec getalldefs e l d=(
          match d with 
          | EmptyDecl -> (e,l) 
          | d -> 
          let (ev,id,d1) = match d with 
          | IntVar(id,d') -> (IVar(l),id,d')
          | BoolVar(id,d') -> (BVar(l),id,d') 
          | _ -> failwith"" in 
          let env' = bind_env e id ev in
          getalldefs env' (l+1) d1) 
      in let (env',loc') = getalldefs env loc d in 
      Cmd(Block(c),(env'::envl,mem,loc'))
(*  | Decl(d,c) -> (  
      match d with 
      | EmptyDecl -> Cmd(Block(c),(env::envl,mem,loc))
      | d -> 
      let (ev,id,d1) = match d with 
      | IntVar(id,d') -> (IVar(loc),id,d')
      | BoolVar(id,d') -> (BVar(loc),id,d') 
      | _ -> failwith"" in 
      let env' = id <-| ev in
      Cmd((Decl(d1,c)),(env'::tenv,mem,(loc+1)))
  )*)
  | Block(c) -> (
      match trace1 (Cmd(c,(envl,mem,loc))) with
      | Cmd(c,st) -> Cmd(Block(c),st)
      | St((_,mem',loc')) -> St((tenv,mem',loc'))
  )

let rec trace_rec n t = 
  if n<=0 then [t]
  else try
    let t' = trace1 t
    in t::(trace_rec (n-1) t')
  with NoRuleApplies -> [t]

let trace n t = trace_rec n (Cmd(t,([bot],bot,0)))

