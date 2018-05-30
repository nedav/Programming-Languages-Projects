(* Other Resources I Consulted: Information about error handling: https://realworldocaml.org/v1/en/html/error-handling.html
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
    (* an integer/bool pattern matches an integer/bool only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
    | (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(i), BoolVal(j)) when i=j -> Env.empty_env()
    | (WildcardPat, _) -> Env.empty_env()  (* No variables are declared in the pattern "_" *)
    | (VarPat(i), j) -> Env.add_binding i j (Env.empty_env()) (* A new variable is declared and is added to the pattern's environment *)
    | (TuplePat(l), TupleVal(k)) -> let rec f l k = 
	    								match l, k with
	    								| [], [] -> (Env.empty_env())
	    								(* Pattern match every corresponding pattern and value of the tuple *)
	    								| h::t, h1::t1 -> Env.combine_envs (patMatch h h1) (f t t1)
	    								| _, _ -> raise (MatchFailure) (* Raise an exception if pattern and value don't have same number of elements *)
    								in f l k
    (* Constructors can only match if they have the same name *)
    | (DataPat(s1, x), DataVal(s2, y)) when s1=s2 -> (match x, y with
    												 | Some a, Some b -> patMatch a b  (* Pattern check the values of the constructors *)
    												 | None, None -> Env.empty_env()   (* No variables are declared with just a C *)
    												 | _, _ -> raise (MatchFailure))
    | _ -> raise (MatchFailure)


(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)


let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
    (* an integer or bool constant evaluates to itself *)
    | IntConst(i) -> IntVal(i)
    | BoolConst(i) -> BoolVal(i)
    (* Lookup the variable in the environment, if it isn't bound, return an error *)
    | Var(i) -> (try (Env.lookup i env)
    			with Env.NotBound -> raise (DynamicTypeError "Unbound value"))
    (* Match the operator, and if the operands are integers, perfrom the operator on them *)
    | BinOp(e1, o, e2)-> (let exp1 = (evalExpr e1 env) in 
    					  let exp2 = (evalExpr e2 env) in
    					  match o with 
    					  | Plus -> (match exp1, exp2 with
    					 			| IntVal(i1), IntVal (i2) -> IntVal(i1 + i2)
    					 			| _ -> raise (DynamicTypeError "Can only add integers"))
    					  | Minus -> (match exp1, exp2 with
    					   			| IntVal(i1), IntVal (i2) -> IntVal(i1 - i2)
    					 			| _ -> raise (DynamicTypeError "Can only subtract integers"))
    					  | Times -> (match exp1, exp2 with
    					 			| IntVal(i1), IntVal (i2) -> IntVal(i1 * i2)
    					 			| _ -> raise (DynamicTypeError "Can only multiply integers"))
    					  | Eq -> (match exp1, exp2 with
    					 			| IntVal(i1), IntVal (i2) -> BoolVal(i1 = i2)
    					 			| _ -> raise (DynamicTypeError "Can only compare integers"))
    					  | Gt -> (match exp1, exp2 with
    					 			| IntVal(i1), IntVal (i2) -> BoolVal(i1 > i2)
    					 			| _ -> raise (DynamicTypeError "Can only compare integers")))
    (* Evaluate the integer constant and negate it *)
    | Negate(i) -> (let exp = (evalExpr i env) in 
    				match exp with
    				| IntVal(x) -> IntVal(-x)
    				| _ -> raise (DynamicTypeError "Can only negate integers")) 
    (* Evaluate the predicate and determine which expression to evaluate based on the result *)
    | If(e1, e2, e3) -> (let e1 = (evalExpr e1 env) in
    					 match e1 with
    					 | BoolVal(i) -> (if i=true then (evalExpr e2 env) else (evalExpr e3 env))
    					 | _ -> raise (DynamicTypeError "Predicate must be boolean")) 
    (* A function expression, declared without let rec, has no name *)
    | Function(param, e) -> FunctionVal (None, param, e, env)
    | FunctionCall(func_e, arg) -> let arg_value = (evalExpr arg env) in 
    								(match func_e with 
    								(* Evaluate the argument, check whether the argument matches with the parameter, and evaluate the function expression *)
    								| Function(param, e) -> evalExpr e (patMatch param arg_value)
    								(* Lookup the function, evaluate the argument, check whether the argument matches with the parameter, combine the environments, and evaluate the function expression *)
    								| Var(i) -> (match (try (Env.lookup i env)
    													with Env.NotBound ->  raise (DynamicTypeError "Unbound value")) with
    								   		    | FunctionVal(name, param, e, func_env) -> evalExpr e (Env.combine_envs env (Env.combine_envs func_env (patMatch param arg_value)))
    								   			| _ -> raise (DynamicTypeError "Unexpected expression for a function call"))
    								(* Evaluate the FunctionCall, if it returns a Function, evaluate that function by 
    								checking whether the argument matches with the parameter, combining the environments, and evaluating the function expression*)
    								| FunctionCall(func_e2, arg2) -> (match (evalExpr func_e env) with 
    																  | FunctionVal (name, param, e, env2) -> evalExpr e (Env.combine_envs env2 (patMatch param arg_value))
    																  | x -> x)
     								| _ -> raise (DynamicTypeError "Unexpected expression for a function call")) 
    (* Evaluate the expression to match and check whether it matches with any patterns *)
    | Match(e, lines) -> let value = (evalExpr e env) in 
    					 let rec f lines =
	    					 match lines with
	    					 | [] -> raise (MatchFailure)  (* If the value doesn't match any of the patterns, return an exception *)
	    					 | (p1, e1)::t -> (try (e1, (patMatch p1 value))  (* Check whether the pattern matches the expression*)
	    					 		   		   with MatchFailure -> f t) (* If it does match, evaluate the expression with the given environment *)
	    					in (match (f lines) with (* Evaluate the match's expression *)
	    						| ex, envx -> evalExpr ex (Env.combine_envs env envx)) 		
	(* Evaluate every element of the tuple and put it in a list *)	  
    | Tuple(l) -> let rec f l = 
    				match l with
    				| [] -> []
    				| h::t -> (evalExpr h env)::(f t)
    			in TupleVal(f l)
    (* Evaluate the moexpr contained in the option and put the resulting movalue into an option *)
    | Data(s, x) -> let value = (match x with
    							| Some y -> Some (evalExpr y env)
    							| None -> None)
						in DataVal(s, value)


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
    (* a top-level expression has no name and is evaluated to a value *)
    | Expr(e) -> (None, evalExpr e env)
    (*A let declaration has a name and is evaluated to a value*)
    | Let (s, e) -> (Some s, evalExpr e env)
    (* Add a mapping of the name of the function to the function val in the environment, then evaluate the function *)
    | LetRec (s, func_e) -> (Some s, evalExpr func_e 
    								 (match func_e with 
    								  | Function(param, e) -> Env.add_binding s (FunctionVal(Some s, param, e, env)) env
    								  | _ -> raise (DynamicTypeError "Unexpected expression for a function declaration")))

