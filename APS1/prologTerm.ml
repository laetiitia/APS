open Ast

let rec print_expr e =
	match e with
	 AstNum n -> Printf.printf"num(%d)" n
	 | AstId x -> Printf.printf"var(%s)" x
	 | AstPrim(op, es) -> (
		  Printf.printf"%s" (string_of_oprim op);
		  Printf.printf"(";
		  print_exprs es;
		  Printf.printf")"
    )
  | AstUnary(op, e) -> (
      Printf.printf"%s" (string_of_opUnary op);
      Printf.printf"(";
      print_expr e;
      Printf.printf")"
    )
  | AstTrue -> Printf.printf"true"
  | AstFalse -> Printf.printf"false"
  | AstIf(cond, body, alter) -> (
      Printf.printf"if(";
      print_expr cond;
      Printf.printf",";
      print_expr body;
      Printf.printf",";
      print_expr alter;
      Printf.printf")"
    )
  | AstAbstraction(args, e) -> (
      Printf.printf"abst([";
      print_args args;
      Printf.printf"],";
      print_expr e;
      Printf.printf")"
    )
  | AstApply(e, es) -> (
      Printf.printf"apply(";
      print_expr e;
      Printf.printf",[";
      print_exprs es;
      Printf.printf"])"
    )
	and print_exprs es =
	  match es with
		[] -> ()
		| [e] -> print_expr e
		| e::es -> (
			print_expr e;
			print_char ',';
			print_exprs es
    )
  and print_type t =
    match t with
      AstTypeInt -> Printf.printf "int"
    | AstTypeBool -> Printf.printf "bool"
    | AstTypeFun(ts, t) -> (
        Printf.printf"typefun([";
        print_types ts;
        Printf.printf"],";
        print_type t;
        Printf.printf")";
      )
    | AstVoid ->  Printf.printf "void"
  and print_types ts =
    match ts with
      AstType t -> print_type t;
    | AstTypes (t, ts) -> (
        print_type t;
        Printf.printf",";
        print_types ts
      )
  and print_arg a =
    match a with
      (ident, t) -> (
        Printf.printf"(" ;
        print_expr ident;
        Printf.printf ",";
        print_type t;
        Printf.printf")"
      )
  and print_args args =
    match args with
      AstArg a -> print_arg a
    | AstArgs(arg , args) -> (
        print_arg arg;
        Printf.printf ",";
        print_args args
      )
    and print_stat stat =
      match stat with
        AstEcho e -> (
          Printf.printf "echo(";
          print_expr e; 
          Printf.printf ")"
        )
        | AstSet (id, e)-> (
          Printf.printf "set(";
          print_expr id; 
          Printf.printf ",";
          print_expr e;
          Printf.printf ")"
        )
        | AstIF (cond, prog1, prog2)-> (
          Printf.printf "ifprog(";
          print_expr cond; 
          Printf.printf ",";
          print_prog prog1;
          Printf.printf ",";
          print_prog prog2;
          Printf.printf ")"
        )
        | AstWhile (cond, block) -> (
          Printf.printf "while(";
          print_expr cond; 
          Printf.printf ",";
          print_prog block;
          Printf.printf ")"
        )
        | AstCall (id, exprs) -> (
          Printf.printf "call(";
          print_expr id; 
          Printf.printf ",";
          print_exprs exprs;
          Printf.printf ")"
        )
    and print_dec dec =
      match dec with 
        AstConst (id, t, e) -> (
          Printf.printf "const(";
          print_expr id;
          Printf.printf ",";
          print_type t;
          Printf.printf ",";
          print_expr e;
          Printf.printf ")"
        )   
        | AstFun (id, t, args, e ) -> (
          Printf.printf "fun(";
          print_expr id;
          Printf.printf ",";
          print_type t;
          Printf.printf ",[";
          print_args args;
          Printf.printf "],";
          print_expr e;
          Printf.printf ")"
        ) 
        | AstFunRec (id, t, args, e) -> (
          Printf.printf "funrec(";
          print_expr id;
          Printf.printf ",";
          print_type t;
          Printf.printf ",[";
          print_args args;
          Printf.printf "],";
          print_expr e;
          Printf.printf ")"
        ) 
        | AstVar (id, t) -> (
          Printf.printf "vardec(";
          print_expr id; 
          Printf.printf ",";
          print_type t;
          Printf.printf ")"
        )
        | AstProc (id, args, block) ->(
          Printf.printf "proc(";
          print_expr id; 
          Printf.printf ",";
          print_args args;
          Printf.printf ",";
          print_prog block;
          Printf.printf ")"
        )
        | AstProcRec (id, args, block) ->(
          Printf.printf "procrec(";
          print_expr id; 
          Printf.printf ",";
          print_args args;
          Printf.printf ",";
          print_prog block;
          Printf.printf ")"
        )
    and print_cmds cmd =
      match cmd with
        AstStat (s) -> (
            Printf.printf "stat(";
            print_stat s;
            Printf.printf ")"
        )
        | AstDec (d, cmds) -> (
            Printf.printf "dec(";
            print_dec d;
            Printf.printf ")";
            Printf.printf ",";
            print_cmds cmds
          )
        | AstStats (s, cmds) -> (
            Printf.printf "stat(";
            print_stat s;
            Printf.printf ")";
            Printf.printf ",";
            print_cmds cmds
        )
    and print_prog prog =
      match prog with 
        AstProg (c) -> (
           Printf.printf "prog([" ;
           print_cmds c;
           Printf.printf "])";
        )
;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
	try
   let lexbuf = Lexing.from_channel ic in
   let p = Parser.prog Lexer.token lexbuf in
     print_prog p;
     print_char '.';
	   print_char '\n'
  with Lexer.Eof ->
	exit 0
