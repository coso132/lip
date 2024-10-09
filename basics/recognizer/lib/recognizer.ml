let rec lang1 c = 
  match c with
  | '0'::[] | '1'::[] -> true
  | '0'::t | '1'::t -> lang1 t
  | _ -> false
;;

let rec lang21 c =
  match c with
  | [] -> true
  | '1'::t-> lang21 t
  | _ -> false
;;

let rec lang2 c = 
  match c with
  | '0'::t -> 
      (match t with
      |'0'::_ -> false
      | t2 -> lang2 t2)
  | '1'::t -> lang21 t
  | [] -> true
  | _ -> false
;;

let rec lang32 c =
  match c with
  |'0'::[]-> true
  |'0'::t|'1'::t -> lang32 t
  |_ -> false
;;

let lang3 c = 
  match c with
  | '0'::t -> lang32 t  
  | _ -> false
;;

let rec lang42 c =
  match c with
  |[] -> true
  | '0'::t -> lang42 t
  | _ -> false
;;
let rec lang41 c = 
  match c with 
  | '0'::t -> lang41 t
  | '1'::t -> lang42 t
  | _ -> false
;;
let rec lang4 c = 
  match c with 
  | '0'::t -> lang4 t
  | '1'::t -> lang41 t
  | _ -> false
;;

let rec lang5 c =
  match c with
  | '0'::'0'::t | '1'::'1'::t 
      -> (match t with 
        []-> true 
      | _ -> lang5 t)
  | _ -> false
;;

let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
