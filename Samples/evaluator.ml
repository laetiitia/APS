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

let eval_expr env e =
  match e with
  | AstNum(x) -> inN(x)
  | AstTrue -> inN(1)
  | AstFalse -> inN(0)
  | AstId(id) -> List.assoc id env
  | AstPrim(op l) -> eval_oprim op (eval_expr (list.hd l)) (eval_expr (list.nth l 1))
  | AstUnary(op e) -> eval_opUnary op (eval_expr e)
  | AstIf(cond, body, alt) -> if (eval_expr cond) == 1 then (eval_expr body) else (eval_expr alt)
  | AstAbstaction(args, e) -> 
  | AstApply(e, list) -> 
  | _ -> failwith "not an Expression"

let eval_prog p =
  match p with
  | AstProg(x) -> eval_cmds [] x
  | _ -> failwith "not a program"

let eval_cmds env cmd =
  match cmd with
  | AstStat(x) -> eval_stat env x
  | AstDec( x , y) -> let newEnv = eval_dec env x in eval_cmds y newEnv
  | AstStats ( x , y) -> eval_stat env x ; eval_cmds y env 
  | _ -> failwith "not a commande"

let eval_dec env dec =
  match dec with
  | AstConst(e1, t, e2) -> ((eval_expr e1),(eval_expr e2))::env
  | AstFun(e1, t, a, e2) -> let newEnv =     in eval_expr newEnv e2
  | AstFunRec(e1, t, a, e2) -> 
  | _ -> failwith "not an AstDec"

let eval_stat env stat =
  match stat with
  | AstEcho(e) -> eval_expr env e
  | _ -> failwith "not an AstStat"

