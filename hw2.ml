(*
Websites used: http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
			   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html				
*)

(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)            
type vector = float list                                 
type matrix = vector list

(* List.map2 adds float1_i and float2_i of either vector *)
let (vplus : vector -> vector -> vector) =
  fun v1 v2 ->
  List.map2 (+.) v1 v2;;

(* List.map2 takes vector1_i and vector2_i of either matrix and adds their elements with vplus *)
let (mplus : matrix -> matrix -> matrix) =
  fun m1 m2 ->
  List.map2 (fun x y -> vplus x y) m1 m2;;

(* List.map2 multiples the float1_i and float2_i of either vector, 
List.fold_left adds up the resulting list of multiplications *)
let (dotprod : vector -> vector -> float) =
  fun v1 v2 ->
  List.fold_left (+.) 0. (List.map2 ( *. ) v1 v2);;

(* Returns the nth column of matrix m *)
let getcolumn m n = 
	List.map (fun row -> List.nth row n) m;;

(* (List.map (fun x -> []) (List.nth m 0)) creates an empty matrix with n rows, where n is the number of columns in m.
   Then for row i of the empty matrix, insert column i *)
let (transpose : matrix -> matrix) =
  fun m ->
  List.mapi (fun idx x -> getcolumn m idx) (List.map (fun x -> []) (List.nth m 0));;

(* Transpose the second matrix so that you can dot product the row of m1 by the row of m2, which is the equivalent to the column
For each vector in m1, calculate the dot product of it and every other vector in m2, storing the result in a new vector *)
let (mmult : matrix -> matrix -> matrix) =
  fun m1 m2 ->
  let transposedm2 = transpose m2 in
  List.map (fun vec1 -> List.map (fun vec2 -> dotprod vec1 vec2) transposedm2) m1;;
        
(* Problem 2: Calculators *)           
           
(* a type for arithmetic expressions *)
type op = Plus | Minus | Times | Divide
type exp = Num of float | BinOp of exp * op * exp

(* Recursively evaluate either expression *)
let rec (evalExp : exp -> float) =
  fun e ->
  match e with
  | Num x -> x
  | BinOp (exp1, o, exp2) -> 
  		match o with
  		| Plus -> (evalExp exp1) +. (evalExp exp2)
  		| Minus -> (evalExp exp1) -. (evalExp exp2)
  		| Times -> (evalExp exp1) *. (evalExp exp2)
  		| Divide -> (evalExp exp1) /. (evalExp exp2);;

(* a type for stack instructions *)	  
type instr = Push of float | Swap | Calculate of op

let (execute : instr list -> float) =
  fun l ->
  let rec helper l stack =
  	match l with
  	| [] -> (match stack with   (* When there are no more instructions, return head of stack *)
  			| h::_ -> h)
  	| h::t -> 
  		match h with
  		| Push x -> helper t (x::stack)  (* Add x to stack and continue to the next instruction *)
  		| Swap -> (match stack with      (* Switch top two elements of stack and continue to the next instruction*)
  				  | h1::h2::t1 -> (helper t (h2::h1::t1)))
  		| Calculate o -> (match stack with   (* Evaluate the result of the top two elements of the stack and the operator, put the result onto the stack, and 
  												continue to the next instruction *)
  				  		 | h1::h2::t1 -> (helper t ((evalExp (BinOp (Num h2, o, Num h1)))::t1)))
  in helper l [];; (* Start with an empty stack*)

(* Postorder: left, right, current *)
let rec (compile : exp -> instr list) =
  fun e ->
  match e with
  | Num x -> [Push x]
  | BinOp (exp1, o, exp2) -> (compile exp1)@(compile exp2)@[Calculate o];;  

let (decompile : instr list -> exp) =
  fun i ->
  let rec helper i arith =
  	match i with
  	| [] -> (match arith with   (* When there are no more instructions, return final expression *)
  			| h::_ -> h)
 	| h::t -> 
  		match h with
  		| Push x -> helper t ((Num x)::arith) (* Add x to expression as a Num and continue to the next instruction *)
  		| Swap -> (match arith with      (* Switch top two numbers of expression and continue to the next instruction*)
  				  | h1::h2::t1 -> (helper t (h2::h1::t1)))
  		| Calculate o -> (match arith with   (* Put the top two Nums of the expression into a BinOp with the operator, put the result in the expression, and 
  												continue to the next instruction *)
  				  		 | h1::h2::t1 -> (helper t ((BinOp (h2, o, h1))::t1)))
 in helper i [];;

(* EXTRA CREDIT *)        

(* Returns the max stack size necessary for a given instr list *)
let rec getStackSize l = 
	let rec helper l curstack maxstack =
	match l with
	| [] -> maxstack
	| h::t -> match h with
			  | Push x -> let c = curstack + 1 in helper t c (max c maxstack)   (* Push adds 1 to the current stack size, update max stack size if applicable *)
			  | Calculate o -> helper t (curstack - 1) (max curstack maxstack)  (* Calculate removes 2 and adds 1 to the current stack size, update max stack size if applicable *)
			  | Swap -> helper t curstack maxstack								(* Swap doesn't change current stack size *)
	in helper l 0 0;;

let rec (compileOpt : exp -> (instr list * int)) =
	fun e ->
  	match e with
  	| Num x -> ([Push x], 1)
	| BinOp (exp1, o, exp2) -> 
		let cmpexp1 = (compileOpt exp1) in   (* Get optimal list of instructions for first expression *)
  		let cmpexp2 = (compileOpt exp2) in   (* Get optimal list of instructions for second expression *)
  		let l1 = fst(cmpexp1)@fst(cmpexp2)@[Calculate o] in  (* List to consider: left expression and then right expression *)
  		let l2 = fst(cmpexp2)@fst(cmpexp1)@[Calculate o] in  (* List to consider: right expression and then left expression *)
  		let l2swap = fst(cmpexp2)@fst(cmpexp1)@[Swap]@[Calculate o] in  (* List to consider: right expression and then left expression (with a Swap) *)
  		let left = (l1, getStackSize(l1)) in                            (* Get the stack size for the list with left expression first *)
  			let right = if ((o = Minus) || (o = Divide)) then (l2swap, getStackSize(l2swap)) else (l2, getStackSize (l2)) in   (* Determine whether Swap is necessary when right expression is first, and get the stack size for the list *)
  				if snd (right) < snd (left) then right else left;;   (* Choose the list with the smaller stack size *)
