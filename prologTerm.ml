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
      Printf.printf"(if ";
      print_expr cond;
      Printf.printf" then ";
      print_expr body;
      Printf.printf" else ";
      print_expr alter;
      Printf.printf" )"
    )
  | AstAbstraction(args, e) -> (
      Printf.printf"Abs([ ";
      print_args args;
      Printf.printf"] = ";
      print_expr e;
      Printf.printf")"
    )
  | AstApply(e, es) -> (
      Printf.printf"Apply( ";
      print_expr e;
      Printf.printf"[";
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
        Printf.printf"(";
        print_types ts;
        Printf.printf"->";
        print_type t;
        Printf.printf")";
      )
  and print_types ts =
    match ts with
      AstType t -> print_type t
    | AstTypes (t, ts) -> (
        print_type t;
        Printf.printf" * ";
        print_types ts
      )
  and print_arg a =
    match a with
      (ident, t) -> (
        Printf.printf"arg(%s : " ident;
        print_type t;
        Printf.printf")"
      )
  and print_args args =
    match args with
      AstArg a -> print_arg a
    | AstArgs(arg , args) -> (
        print_arg arg;
        Printf.printf " , ";
        print_args args
      )
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
