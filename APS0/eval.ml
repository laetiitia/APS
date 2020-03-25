open Ast

type valeurs = InN of int 
    | InF of expr * string list * (string * valeurs) list 
    | InFR of string * valeurs
;;

(* FONCTIONS UTILES *)
let eval_int v =
  match v with
  | InN(x) -> x
  | _ -> failwith "not in N"
;;

let eval_oprim op e1 e2 =
  match op with
   "add" -> InN((eval_int e1) + (eval_int e2))
  | "mul" -> InN((eval_int e1) * (eval_int e2))
	| "sub" -> InN((eval_int e1) - (eval_int e2))
	| "div" -> if ((eval_int e2) == 0) then failwith "can't divide with 0" else InN((eval_int e1)/(eval_int e2))
	| "and" -> if ((eval_int e1) == 0) then InN(0) else e2 
	| "or" -> if ((eval_int e1) == 1) then InN(1) else e2
	| "eq" -> if ((eval_int e1) == (eval_int e2)) then InN(1) else InN(0)
  | "lt" -> if ((eval_int e1) < (eval_int e2)) then InN(1) else InN(0)
  | _ -> failwith "not an oprim"
;;

let eval_opUnary op e =
  match op with
  | "not" -> if ((eval_int e) == 0) then InN(1) else InN(0)
  | _ -> failwith "not an opUnary"
;;

let rec eval_expr env e =
  match e with
  | AstNum(x) -> InN(x)
  | AstTrue -> InN(1)
  | AstFalse -> InN(0)
  | AstId(id) -> List.assoc e env 
  | AstPrim(op, l) -> eval_oprim (string_of_oprim op) (eval_expr env (List.hd l)) (eval_expr env (List.nth l 1))
  | AstUnary(op, e) -> eval_opUnary (string_of_opUnary op) (eval_expr env e)
  | AstIf(cond, body, alt) -> if (eval_expr env cond) == InN(1) then (eval_expr env body) else (eval_expr env alt)
  (*| AstAbstaction(args, e) -> 
  | AstApply(e, list) -> *)
  | _ -> failwith "not an Expression"
;;

let eval_stat env stat =
  match stat with
  | AstEcho(e) -> Printf.printf "%d " (eval_int (eval_expr env e))
  | _ -> failwith "not an AstStat"
;; 

let rec eval_dec env dec =
  match dec with
  | AstConst(e1, t, e2) -> (e1,(eval_expr env e2))::env
  (*| AstFun(e1, t, a, e2) -> let newEnv =     in eval_expr newEnv e2
  | AstFunRec(e1, t, a, e2) -> *)
  | _ -> failwith "not an AstDec"
;;

let rec eval_cmds env cmd =
  match cmd with
  | AstStat(x) -> eval_stat env x
  | AstDec( x , y) -> let newEnv = eval_dec env x in eval_cmds newEnv y
  | AstStats ( x , y) -> eval_stat env x ; eval_cmds env y 
  | _ -> failwith "not a commande"
;;

let eval_prog p =
  match p with
  | AstProg(x) -> eval_cmds [] x
  | _ -> failwith "not a program"
;;


  (**           Gestion Env              **)
(*let find env id =*)


let fname = Sys.argv.(1) in
  let ic = open_in fname in
    try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p;
      print_char '\n'
    with Lexer.Eof ->
    exit 0
  