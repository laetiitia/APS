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


let get_string ast =
  match ast with 
  | AstId(id) -> id
  | _ -> failwith "not an ident" 



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

let rec eval_args args =
  match args with 
  | AstArg(e,t) -> (get_string e)::[]
  | AstArgs((e,t),suite) -> (get_string e)::(eval_args suite)
;;


let rec eval_expr env e =
  match e with
  | AstNum(x) -> InN(x)
  | AstTrue -> InN(1)
  | AstFalse -> InN(0)
  | AstId(id) -> List.assoc id env 
  | AstPrim(op, l) -> eval_oprim (string_of_oprim op) (eval_expr env (List.hd l)) (eval_expr env (List.nth l 1))
  | AstUnary(op, e) -> eval_opUnary (string_of_opUnary op) (eval_expr env e)
  | AstIf(cond, body, alt) -> if (eval_int (eval_expr env cond)) == 1 
                              then (eval_expr env body) 
                              else (eval_expr env alt)
  | AstAbstraction(args, e) -> InF(e, (eval_args args), env)
  | AstApply(e, exprs) -> let eval_e = (eval_expr env e) 
                          and liste_exprs = eval_exprs exprs env [] in 
                              (match eval_e with 
                                | InF(expr, args, g) -> let newG = List.append (List.map2 (fun x y -> (x,y)) args liste_exprs) g in eval_expr newG expr
                                | InFR(id, InF(expr,args,g)) -> let newG = let newG = List.append (List.map2 (fun x y -> (x,y)) args liste_exprs) g in 
                                | _ -> failwith "cannot be apply")
and eval_exprs exprs env acc =
  match exprs with 
  | [] -> acc
  | e::es -> (eval_expr env e)::(eval_exprs es env acc)
;;

(*
	|InF(body,params,env1) -> let closure_env = (List.map2 (fun x y -> (x,y)) params args_list)@env1 in eval_expr closure_env body
	|InFR(f,InF(body,params,env1)) -> let closure_env =
				f,List.assoc f env)::(List.map2 (fun x y -> (x,y)) params args_list)@env1 in
								eval_expr closure_env body
	*)

let eval_stat env stat =
  match stat with
  | AstEcho(e) -> Printf.printf "%d " (eval_int (eval_expr env e))
;; 

let rec eval_dec env dec =
  match dec with
  | AstConst(id, t, expr) -> ((get_string id), (eval_expr env expr))::env
  | AstFun(id, t, args, expr) -> ((get_string id), InF(expr, eval_args args, env))::env 
  | AstFunRec(id, t, args, expr) -> (get_string id, InFR(get_string id, InF(expr, eval_args args, env)) )::env
  | _ -> failwith "not an AstDec"
;;

let rec eval_cmds env cmd =
  match cmd with
  | AstStat(stat) -> eval_stat env stat
  | AstDec(dec, cmd) -> let newEnv = eval_dec env dec in eval_cmds newEnv cmd
  | AstStats (stat, cmd) -> eval_stat env stat ; eval_cmds env cmd 
;;

let eval_prog p =
  match p with
  | AstProg(prog) -> eval_cmds [] prog
;;


(*  Lecture du fichier .aps *)

let fname = Sys.argv.(1) in
  let ic = open_in fname in
    try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      eval_prog p;
      print_char '\n'
    with Lexer.Eof ->
    exit 0
  