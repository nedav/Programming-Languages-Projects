(*
Website used: http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
*)
(* Problem 1 *)
          
(* Check every element of the list to see if x is one of them *)  
let rec (member : 'a -> 'a list -> bool) =
 fun x s -> 
  match s with
  | [] -> false
  | h::t -> if h=x then true else (member x t);;

(* Only add x to the list if x isn't already a member of it *)
let (add : 'a -> 'a list -> 'a list) =
  fun x s ->
  if (member x s) then s else x::s;;

(* Add an element of s1 to s2 as long as that element isn't also a member of s2 *)
let rec (union : 'a list -> 'a list -> 'a list) =
  fun s1 s2 ->
  match s1 with
  | [] -> s2
  | h::t -> union t (add h s2);;

(* Starting from the first elements of either list, add the smaller element to the union and increment the list containing it, 
if they are duplicates, add one to the union and increment both lists  *)
let rec (fastUnion : 'a list -> 'a list -> 'a list) =
	fun s1 s2 ->
	match s1, s2 with
	| [], [] -> []
	| [], _::_ -> s2
	| _::_, [] -> s1
	| h1::t1, h2::t2 -> if h1<h2 then [h1]@(fastUnion t1 s2) else if h2<h1 then [h2]@(fastUnion s1 t2) else [h1]@(fastUnion t1 t2);;
                
(* Only keep the elements of s1 that are also in s2 *)
let (intersection : 'a list -> 'a list -> 'a list) =
  fun s1 s2 ->
  List.filter (fun x -> member x s2) s1;;
 
(* For every element of the list, only keep it if it doesn't appear in the rest of the list *)
let rec (setify : 'a list -> 'a list) =
  fun l ->
  match l with
  | [] -> []
  | h::t -> if (member h t) then setify t else h::(setify t);;

(* Helper functions for powerset *)

(* Given an int elem and a list of lists lst, the function adds elem to every list in the list *)
let rec add2 elem lst = 
	match lst with
	| [] -> []
	| h::t -> [h@[elem]]@(add2 elem t);;

(* Function returns the list of lists for one element, which will be built upon *)
let rec initialStep lst = 
	match lst with
	| [] -> []
	| h::_ -> [[]; [h]];;

(* Returns first element of a list (only non-empty lists will be passed into this function *)
let getFirst lst = 
	match lst with
	| h::_ -> h;;

let rec (powerset : 'a list -> 'a list list) =
  fun s ->
  if s=[] then [[]] else
  let first = (getFirst s) in 
  	let rec func set original = (* recurse through inputted list to get every new element to add *)
  	match original with  (* Copy the last list of lists and add the current element to every list in the list *)
  	| [] -> set
 	| h::t -> if h=first then (func (initialStep original) t) else func (set@(add2 h set)) t
 in
	func [] s;;

(* Problem 2 *)        

(* Recurse until the last element of the list, evaluate it, return the resulting pair, and add the preceding elemnents to the return value *)
let rec (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) =
  fun func lst ->
  	match lst with
  	| [] -> ([], [])
  	| [x] -> if func x then ([x], []) else ([], [x])
  	| h::t -> let pair = partition func t in
  		match pair with
  		| (x, y) -> if func h then ([h]@x, y) else (x, [h]@y);; 

(* Recurse until the predicate is false *)
let rec (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) =
  fun p f x ->
  if p x = true then whle p f (f x) else x;;
                                 
(* Recurse until n=1, return the base case function to be the argument for the n=2 function, and iteratively do so for all n functions *)
let rec (pow : int -> ('a -> 'a) -> ('a -> 'a)) =
	fun n f ->
	match n with
	| 0 -> (function v -> v)
	| 1 -> (function v -> f v)
	| x -> let t = pow (x-1) f in (function v -> f (t v));;
