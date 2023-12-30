open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
  (*Value*)
  | Value i -> i
  (*ID*)
  | ID i -> lookup env i
  (*Not*)
  | Not i -> (let i' = eval_expr env i in match i' with
    | Bool x -> Bool (not x)
    | _ -> raise (TypeError("Expected type bool"))
  )
  (*Binop*)
  | Binop (op, x, y) ->
  let x' = eval_expr env x in
  let y' = eval_expr env y in
  (match op with
    | Add -> (match (x', y') with
      | (Int a, Int b) -> Int (a + b)
      | _ -> raise (TypeError("Expected type int")))
    | Sub -> (match (x', y') with
      | (Int a, Int b) -> Int (a - b)
      | _ -> raise (TypeError("Expected type int")))
    | Mult -> (match (x', y') with
      | (Int a, Int b) -> Int (a * b)
      | _ -> raise (TypeError("Expected type int")))
    | Div -> (match (x', y') with
      | (Int a, Int b) -> if b = 0 then raise (DivByZeroError)
          else Int (a / b)
      | _ -> raise (TypeError("Expected type int")))
    | Greater -> (match (x', y') with
      | (Int a, Int b) -> Bool (a > b)
      | _ -> raise (TypeError("Expected type int")))
    | Less -> (match (x', y') with
      | (Int a, Int b) -> Bool (a < b)
      | _ -> raise (TypeError("Expected type int")))
    | GreaterEqual -> (match (x', y') with
      | (Int a, Int b) -> Bool (a >= b)
      | _ -> raise (TypeError("Expected type int")))
    | LessEqual -> (match (x', y') with
      | (Int a, Int b) -> Bool (a <= b)
      | _ -> raise (TypeError("Expected type int")))
    | Concat -> (match (x', y') with
      | (String a, String b) -> String (a ^ b)
      | _ -> raise (TypeError("Expected type string")))
    | Equal -> (match (x', y') with
      | (Int a, Int b) -> Bool (a = b)
      | (Bool a, Bool b) -> Bool (a = b)
      | (String a, String b) -> Bool (a = b)
      | _ -> raise (TypeError("Cannot compare types")))
    | NotEqual -> (match (x', y') with
      | (Int a, Int b) -> Bool (a <> b)
      | (Bool a, Bool b) -> Bool (a <> b)
      | (String a, String b) -> Bool (a <> b)
      | _ -> raise (TypeError("Cannot compare types")))
    | Or -> (match (x', y') with
      | (Bool a, Bool b) -> Bool (a || b)
      | _ -> raise (TypeError("Expected type bool")))
    | And -> (match (x', y') with
      | (Bool a, Bool b) -> Bool (a && b)
      | _ -> raise (TypeError("Expected type bool")))
  )
  
  (*If*)
  | If (guard, trueb, falseb) -> (match eval_expr env guard with
      | Bool x -> if x then (eval_expr env trueb) else (eval_expr env falseb)
      | _ -> raise (TypeError("If error. Invalid type for guard branch."))
  )
  (*Let*)
  | Let (var, recur, init, body) -> (match recur with
    (*Non-recursive Let*)
    | false -> let v = eval_expr env init in (*Evaluate initialization expression to v*)
      (*Create environment extended with a mapping from ID variable var to v*)
      let env' = extend env var v in
        (*Return result of evaluating body expression in this environment*)
        eval_expr env' body
    (*Recursive Let*)
    | true -> let env' = extend_tmp env var in (*Create temporary placeholder for environment extended with a mapping from ID var*)
      (*Evaluate the initialization expression in the environment*)
      let ph = eval_expr env' init in 
        (*Update placeholder to var*)
        update env' var ph;
        (*Evaluate the body*)
        eval_expr env' body 
  )
  
  (*Fun*)
  | Fun (x, y) -> Closure(env, x, y)
  
  (*FunctionCall*)
  | FunctionCall (x, y) -> (
    let x' = eval_expr env x in
    let v = eval_expr env y in
    (*Evaluate first expression to Closure(env, x, e)*)
    match x' with
      | Closure(env, x, e) -> 
        (*Update environment*)
        let env' = extend env x v in
        (*Evaluate e in environment env' and return result*)
        eval_expr env' e 
      | _ -> raise (TypeError "Not a function")
  )

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
  (*Def*)
  (*Create environment extended with mapping from variable*)
  | Def (var, expr) -> let env' = extend_tmp env var in
    (*Evaluate expr in the environment with a placeholder set for variable*)
    let v = eval_expr env' expr in
      (*Update the binding for var to be v*)
      update env' var v;
      (*Return extended environment and value option*)
      (env', Some v)
  (*Expr*)
  (*For a Expr, we should evaluate the expression in the given environment, and return that environment and the resulting value.*)
  | Expr (expr) -> 
  (*Evaluate in given environment*)
    let v = eval_expr env expr in
      (*Return environment and resulting value option*)
      (env, Some v)
  (*NoOp*)
  (*The NoOp should return the original environment and no value (None).*)
  | NoOp -> (env, None)
