(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let addlist l = 
  match l with
  | [] -> 0
  | t::b -> t + addlist b;
