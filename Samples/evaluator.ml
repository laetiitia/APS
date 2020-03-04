open Ast

type valeurs = inN of int (* N *)
    | inF of expr * string list * (string * valeurs) list 
    | inFR of string * valeurs

(* FONCTIONS UTILES *)
let eval_oprim op e1 e2 =
  match op with
  | "add" -> e1 + e2
  | "mul" -> e1 * e2
	| "sub" -> e1 - e2
	| "div" -> if (e2 == 0) then failwith "can't divide with 0" else e1/e2
	| "and" -> if (e1 == 0) then 0 else e2 
	| "or" -> if (e1 == 1) then 1 else e2
	| "eq" -> if (e1 == e2) then 1 else 0
  | "lt" -> if (e1 < e2) then 1 else 0
  | _ -> failwith "not an oprim"

let eval_opUnary op e =
  match op with
  | "not" -> if (e == 0) then 1 else 0
  | _ -> failwith "not an opUnary"


let eval_int v =
  match v with
  | inN(x) -> x
  | _ -> failwith "not in N"

let eval_expr e =
  match e with
  | AstNum(x) -> inN(x)
  | AstTrue -> inN(1)
  | AstFalse -> inN(0)
  | AstId(id) -> 
  | AstPrim(op l) -> eval_oprim op (eval_expr (list.hd l)) (eval_expr (list.nth l 1))
  | AstUnary(op e) -> eval_opUnary op (eval_expr e)
