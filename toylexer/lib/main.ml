open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let rec firstn n l =
  match (n,l) with
  | (n,h::t) when n>0 -> (firstn (n-1) t)@[h] 
  | _ -> []

let rec remove_duplicates l =
  match l with
  | [] -> []
  | h::t -> if( List.exists (fun x -> x=h) t) 
            then remove_duplicates t
            else h::(remove_duplicates t)
          
let frequency (i:int)  (tkl:'a list) = 
  tkl |> List.map( fun x -> (x, 
                            tkl |> List.filter(fun e -> x == e)
                                |> List.length))
      |> List.sort(fun (_,x) (_,y)-> x-y )
      |> remove_duplicates
      |> firstn i
