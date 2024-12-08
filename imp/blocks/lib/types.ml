open Ast
    
type loc = int

type envval = BVar of loc | IVar of loc
type memval = Bool of bool | Int of int

type env = ide -> envval option
type mem = loc -> memval option

(* The third component of the state is the first free location.
   We assume that the store is unbounded *)
type state = env list * mem * loc

let topenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_) = el
let getmem (_,m,_) = m
let getloc (_,_,l) = l

  
type conf = St of state | Cmd of cmd * state
let getstate c = match c with
  | St(st) -> st
  | Cmd(_,st) -> st

let concat_strings lst =
  List.fold_left (fun acc s -> acc ^ s) "" lst


exception TypeError of string
exception UnboundVar of ide
exception NoRuleApplies
