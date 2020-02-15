type token =
  | NUM of (int)
  | IDENT of (string)
  | CONST
  | FUN
  | REC
  | ECHO
  | IF
  | TRUE
  | FALSE
  | INT
  | BOOL
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | AND
  | OR
  | EQ
  | LT
  | NOT
  | LPAR
  | RPAR
  | LCRO
  | RCRO
  | PNTV
  | DPNT
  | VIRG
  | ETOILE
  | FLECHE

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 37 "parser.ml"
let yytransl_const = [|
  259 (* CONST *);
  260 (* FUN *);
  261 (* REC *);
  262 (* ECHO *);
  263 (* IF *);
  264 (* TRUE *);
  265 (* FALSE *);
  266 (* INT *);
  267 (* BOOL *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* TIMES *);
  271 (* DIV *);
  272 (* AND *);
  273 (* OR *);
  274 (* EQ *);
  275 (* LT *);
  276 (* NOT *);
  277 (* LPAR *);
  278 (* RPAR *);
  279 (* LCRO *);
  280 (* RCRO *);
  281 (* PNTV *);
  282 (* DPNT *);
  283 (* VIRG *);
  284 (* ETOILE *);
  285 (* FLECHE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\004\000\005\000\006\000\006\000\006\000\
\007\000\007\000\007\000\008\000\008\000\009\000\010\000\010\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\002\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\003\000\002\000\004\000\007\000\008\000\
\001\000\001\000\005\000\001\000\003\000\003\000\001\000\003\000\
\001\000\001\000\001\000\001\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\006\000\004\000\004\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\017\000\020\000\018\000\019\000\000\000\000\000\
\035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\032\000\009\000\010\000\000\000\014\000\016\000\031\000\
\000\000\000\000\000\000\030\000\000\000\000\000\013\000\000\000\
\011\000"

let yydgoto = "\002\000\
\025\000\026\000\000\000\000\000\000\000\000\000\058\000\059\000\
\022\000\023\000"

let yysindex = "\006\000\
\003\255\000\000\000\000\000\000\000\000\000\000\027\255\006\255\
\000\000\003\255\003\255\003\255\003\255\003\255\003\255\003\255\
\003\255\003\255\003\255\003\255\240\254\243\254\247\254\003\255\
\003\255\250\254\251\254\253\254\254\254\255\254\000\255\008\255\
\009\255\010\255\011\255\248\254\006\255\003\255\003\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\248\254\000\000\000\000\000\000\
\036\255\021\255\252\254\000\000\248\254\248\254\000\000\038\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\037\255\000\000\000\000\
\040\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\034\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\255\255\039\000\000\000\000\000\000\000\000\000\221\255\004\000\
\000\000\029\000"

let yytablesize = 66
let yytable = "\009\000\
\054\000\051\000\052\000\003\000\004\000\020\000\001\000\021\000\
\024\000\036\000\005\000\006\000\053\000\037\000\038\000\041\000\
\042\000\034\000\043\000\044\000\045\000\046\000\039\000\007\000\
\062\000\008\000\064\000\003\000\004\000\047\000\048\000\049\000\
\050\000\010\000\005\000\006\000\056\000\057\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\007\000\
\061\000\008\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\060\000\035\000\065\000\015\000\033\000\012\000\040\000\
\063\000\055\000"

let yycheck = "\001\000\
\036\000\010\001\011\001\001\001\002\001\007\000\001\000\002\001\
\010\000\026\001\008\001\009\001\021\001\027\001\024\001\022\001\
\022\001\019\000\022\001\022\001\022\001\022\001\024\000\021\001\
\029\001\023\001\062\000\001\001\002\001\022\001\022\001\022\001\
\022\001\007\001\008\001\009\001\038\000\039\000\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\028\001\023\001\012\000\013\000\014\000\015\000\016\000\017\000\
\018\000\022\001\020\000\022\001\024\001\022\001\029\001\025\000\
\061\000\037\000"

let yynames_const = "\
  CONST\000\
  FUN\000\
  REC\000\
  ECHO\000\
  IF\000\
  TRUE\000\
  FALSE\000\
  INT\000\
  BOOL\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  NOT\000\
  LPAR\000\
  RPAR\000\
  LCRO\000\
  RCRO\000\
  PNTV\000\
  DPNT\000\
  VIRG\000\
  ETOILE\000\
  FLECHE\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 30 "parser.mly"
                   ( AstProg(_2) )
# 193 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 34 "parser.mly"
                   ( AstStat(_1) )
# 200 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 35 "parser.mly"
                    ( AstDec(_1, _3) )
# 208 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 36 "parser.mly"
                    ( AstStats(_1, _3) )
# 216 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 40 "parser.mly"
                    ( AstEcho(_2) )
# 223 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.type_) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 44 "parser.mly"
                                              ( AstConst(_2, _3, _4) )
# 232 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.type_) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 45 "parser.mly"
                                              ( AstFun(_2, _3, _5, _7) )
# 242 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.type_) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                                              ( AstFunRec(_3, _4, _6, _8) )
# 252 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
        ( AstTypeInt )
# 258 "parser.ml"
               : Ast.type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
         ( AstTypeBool )
# 264 "parser.ml"
               : Ast.type_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.type_) in
    Obj.repr(
# 52 "parser.mly"
                                 ( AstTypeFun(_2, _4) )
# 272 "parser.ml"
               : Ast.type_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.type_) in
    Obj.repr(
# 56 "parser.mly"
                      ( AstType(_1) )
# 279 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.type_) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.types) in
    Obj.repr(
# 57 "parser.mly"
                       ( AstTypes(_1, _3) )
# 287 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.type_) in
    Obj.repr(
# 61 "parser.mly"
                      ( (_1, _3) )
# 295 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 65 "parser.mly"
                 ( AstArg(_1) )
# 302 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 66 "parser.mly"
                  ( AstArgs(_1, _3) )
# 310 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "parser.mly"
                              ( AstNum(_1) )
# 317 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                               ( AstTrue )
# 323 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                               ( AstFalse )
# 329 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                               ( AstId(_1) )
# 336 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 73 "parser.mly"
                               ( AstPrim(Ast.Add, _3) )
# 343 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 74 "parser.mly"
                               ( AstPrim(Ast.Sub, _3) )
# 350 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 75 "parser.mly"
                               ( AstPrim(Ast.Mul, _3) )
# 357 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 76 "parser.mly"
                               ( AstPrim(Ast.Div, _3) )
# 364 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 77 "parser.mly"
                               ( AstPrim(Ast.And, _3) )
# 371 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 78 "parser.mly"
                               ( AstPrim(Ast.Or, _3) )
# 378 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 79 "parser.mly"
                               ( AstPrim(Ast.Eq, _3) )
# 385 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 80 "parser.mly"
                               ( AstPrim(Ast.Lt, _3) )
# 392 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                               ( AstUnary(Ast.Not, _3) )
# 399 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                               ( AstIf(_3, _4, _5) )
# 408 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
                               ( AstAbstraction(_2, _4) )
# 416 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 84 "parser.mly"
                               ( AstApply(_2, _3) )
# 424 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
                  ( [_1] )
# 431 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 89 "parser.mly"
                  ( _1::_2 )
# 439 "parser.ml"
               : Ast.expr list))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
