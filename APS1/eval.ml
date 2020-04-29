open Ast

type valeurs = 
      InN of int 
    | InF of expr * string list * (string * valeurs) list 
    | InFR of string * valeurs
    | InA of int 
    | InP of prog * string list * (string * valeurs) list 
    | InPR of string * valeurs
;;

(* FONCTIONS APS1 *)
let adress = ref 0
let alloc mem = let mem' = (!adress, ( (!adress, ref (InN(-1))) ::mem) ) in 
                    adress := (!adress +1); 
                    mem'


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
   "add" -> InN((eval_int e1 ) + (eval_int e2 ))
  | "mul" -> InN((eval_int e1 ) * (eval_int e2 ))
	| "sub" -> InN((eval_int e1 ) - (eval_int e2 ))
	| "div" -> if ((eval_int e2 ) == 0) then failwith "can't divide with 0" else InN((eval_int e1 )/(eval_int e2 ))
	| "and" -> if ((eval_int e1 ) == 0) then InN(0) else e2 
	| "or" -> if ((eval_int e1 ) == 1) then InN(1) else e2
	| "eq" -> if ((eval_int e1 ) == (eval_int e2 )) then InN(1) else InN(0)
  | "lt" -> if ((eval_int e1 ) < (eval_int e2 )) then InN(1) else InN(0)
  | _ -> failwith "not an oprim"
;;

let eval_opUnary op e =
  match op with
  | "not" -> if ((eval_int e ) == 0) then InN(1) else InN(0)
  | _ -> failwith "not an opUnary"
;;

let rec eval_args args =
  match args with 
  | AstArg(e,t) -> (get_string e)::[]
  | AstArgs((e,t),suite) -> (get_string e)::(eval_args suite)
;;


let rec eval_expr env mem e =
  match e with
  | AstNum(x) -> InN(x)
  | AstTrue -> InN(1)
  | AstFalse -> InN(0)
  | AstId(id) ->( match (List.assoc id env ) with
                  | InA(x) -> !(List.assoc x mem)
                  | x -> x ) 
  | AstPrim(op, l) -> eval_oprim (string_of_oprim op) (eval_expr env mem (List.hd l)) (eval_expr env mem (List.nth l 1))
  | AstUnary(op, e) -> eval_opUnary (string_of_opUnary op) (eval_expr env mem e)
  | AstIf(cond, body, alt) -> if (eval_int (eval_expr env mem cond) ) == 1 
                              then (eval_expr env mem body) 
                              else (eval_expr env mem alt)
  | AstAbstraction(args, e) -> InF(e, (eval_args args), env)
  | AstApply(e, exprs) -> let eval_e = (eval_expr env mem e) 
                          and liste_exprs = eval_exprs exprs env mem [] in 
                              (match eval_e with 
                                | InF(expr, args, g) -> let newG = List.append (List.map2 (fun x y -> (x,y)) args liste_exprs) g in eval_expr newG mem expr
                                | InFR(id, InF(expr, args, g))-> let newG = List.append (List.map2 (fun x y -> (x,y)) args liste_exprs) g in eval_expr  (( id ,InFR(id, InF(expr, args, g)))::newG) mem expr
                                | _ -> failwith "cannot be apply")
and eval_exprs exprs env mem acc =
  match exprs with 
  | [] -> acc
  | e::es -> (eval_expr env mem e)::(eval_exprs es env mem acc)
;;

(* Return couple memoire et flux de retour*)
let rec eval_stat env mem out stat =
  match stat with
  | AstEcho(e) -> let result = (eval_int (eval_expr env mem e) ) in 
                    out:=!out ^ (string_of_int result) ^ "\n";
                    (mem, out)

  | AstSet(id,e) -> let adr = List.assoc (get_string id) env in
                      (match adr with
                        InA(a) -> let x = (List.assoc a mem) and 
                                      res = eval_expr env mem e in
                                        x := res;
                                        (mem, out)
                        |_ -> failwith "Error AstSet : wrong id")

  | AstIF(cond, body, alt) -> if (eval_int (eval_expr env mem cond)) == 1 
                              then (eval_prog body env mem)
                              else (eval_prog alt env mem)

  | AstWhile(cond, prog) -> if (eval_int (eval_expr env mem cond)) == 1 
                            then let (mem', out')= (eval_prog prog env mem) in
                                    eval_stat env mem' out' stat
                            else (mem, out)
  | AstCall(expr, exprs) -> let eval_e = (eval_expr env mem expr) 
                              and liste_exprs = eval_exprs exprs env mem [] in 
                                  (match eval_e with 
                                    | InP(prog, args, env1) -> let newG = List.append (List.map2 (fun x y -> (x,y)) args liste_exprs) env1 in eval_prog prog newG mem
                                    | InPR(id, InP(prog, args, env1))-> let newG = List.append (List.map2 (fun x y -> (x,y)) args liste_exprs) env1 in eval_prog prog (( id ,InPR(id, InP(prog, args, env1)))::newG) mem
                                    | _ -> failwith "cannot be apply")
(* Return couple environnement et memoire *)
and eval_dec env mem dec =
  match dec with
  | AstConst(id, t, expr) -> ((get_string id, (eval_expr env mem expr))::env , mem)
  | AstFun(id, t, args, expr) -> ((get_string id, InF(expr, eval_args args, env))::env , mem)
  | AstFunRec(id, t, args, expr) -> ((get_string id, InFR(get_string id, InF(expr, eval_args args, env)) )::env , mem)
  | AstVar(id, t) -> let (adr, mem') = alloc(mem) in ((get_string id, InA(adr))::env, mem')
  | AstProc(id, args, prog) -> ( (get_string id, InP(prog, eval_args args, env) )::env, mem )
  | AstProcRec(id, args, prog) -> ( (get_string id, InPR(get_string id, InP(prog, eval_args args, env)))::env, mem )
  | _ -> failwith "not an AstDec"


(* Return couple memoire et flux de retour*)
and eval_cmds env mem out cmd =
  match cmd with
  | AstStat(stat) -> eval_stat env mem out stat
  | AstDec(dec, cmd) -> let (env', mem') = eval_dec env mem dec in 
                          eval_cmds env' mem' out cmd
  | AstStats (stat, cmd) -> let (mem', out') = eval_stat env mem out stat in 
                              eval_cmds env mem' out' cmd 

(* Return couple memoire et flux de retour*)
and eval_prog p env mem =
  match p with
  | AstProg(prog) -> let out = ref "Sortie: \n" in
                        eval_cmds env mem out prog 
;;


(*  Lecture du fichier .aps *)

let fname = Sys.argv.(1) in
  let ic = open_in fname in
    try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    let (memory, out) = eval_prog p [] [] in 
       Printf.printf "%s" !out
    with Lexer.Eof ->
    exit 0