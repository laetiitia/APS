open Ast

let rec print_expr e =
	match e with
	 AstNum n -> Printf.printf"num(%d)" n
	 | AstId x -> Printf.printf"var(%s)" x
	 | AstPrim(op, es) -> (
		  Printf.printf"%s" (string_of_oprim op);
		  Printf.printf"([";
		  print_exprs es;
		  Printf.printf"])"
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
      Printf.printf"( ";
      print_expr cond;
      Printf.printf" -> ";
      print_expr body;
      Printf.printf" ; ";
      print_expr alter;
      Printf.printf" )"
    )
  | AstAbstraction(args, e) -> (
      Printf.printf"Abs([ ";
      print_args args;
      Printf.printf"]";
      print_exprs e;
      Printf.printf")"
    )
  | AstApply(e, es) -> (
      Printf.printf"( ";
      print_expr e;
      Printf.printf"([";
      print_exprs es;
      Printf.printf"]))"
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
  and print_arg ident type =  
		;;
let fname = Sys.argv.(1) in
let ic = open_in fname in
	try
   let lexbuf = Lexing.from_channel ic in
   let e = Parser.expr Lexer.token lexbuf in
	   print_expr e;
	   print_char '\n'
  with Lexer.Eof ->
	exit 0
