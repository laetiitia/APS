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
  | VAR
  | PROC
  | SET
  | IFPROG
  | WHILE
  | CALL
  | VOID

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 44 "parser.ml"
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
  286 (* VAR *);
  287 (* PROC *);
  288 (* SET *);
  289 (* IFPROG *);
  290 (* WHILE *);
  291 (* CALL *);
  292 (* VOID *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\004\000\005\000\005\000\005\000\005\000\
\005\000\006\000\006\000\006\000\006\000\006\000\006\000\007\000\
\007\000\007\000\007\000\008\000\008\000\009\000\010\000\010\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\002\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\003\000\002\000\003\000\004\000\003\000\
\003\000\004\000\007\000\008\000\003\000\006\000\007\000\001\000\
\001\000\005\000\001\000\001\000\003\000\003\000\001\000\003\000\
\001\000\001\000\001\000\001\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\006\000\004\000\004\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\028\000\026\000\027\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\016\000\017\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\000\000\006\000\000\000\008\000\000\000\
\009\000\004\000\003\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\042\000\000\000\000\000\000\000\000\000\000\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\040\000\022\000\024\000\039\000\000\000\000\000\021\000\000\000\
\000\000\000\000\000\000\014\000\000\000\018\000\011\000\000\000\
\038\000\015\000\012\000"

let yydgoto = "\002\000\
\064\000\065\000\004\000\014\000\015\000\016\000\068\000\069\000\
\056\000\057\000"

let yysindex = "\002\000\
\242\254\000\000\018\255\000\000\026\255\014\255\054\255\031\255\
\024\255\034\255\054\255\054\255\037\255\252\254\017\255\019\255\
\004\255\004\255\041\255\000\000\000\000\000\000\000\000\082\255\
\043\255\000\000\004\255\035\255\045\255\054\255\242\254\242\254\
\054\255\000\000\018\255\018\255\000\000\000\000\004\255\000\000\
\054\255\036\255\004\255\054\255\054\255\054\255\054\255\054\255\
\054\255\054\255\054\255\054\255\054\255\054\255\028\255\030\255\
\040\255\000\000\043\255\038\255\000\000\242\254\000\000\054\255\
\000\000\000\000\000\000\032\255\042\255\000\000\043\255\044\255\
\054\255\047\255\048\255\050\255\051\255\052\255\056\255\058\255\
\059\255\060\255\064\255\004\255\043\255\054\255\063\255\043\255\
\000\000\000\000\004\255\004\255\068\255\043\255\054\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\242\254\080\255\000\000\085\255\
\054\255\084\255\087\255\000\000\242\254\000\000\000\000\054\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\086\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\088\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\244\254\
\000\000\000\000\000\000\093\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\249\255\069\000\232\255\251\255\000\000\000\000\240\255\230\255\
\000\000\203\255"

let yytablesize = 133
let yytable = "\026\000\
\041\000\042\000\001\000\031\000\032\000\087\000\062\000\063\000\
\003\000\041\000\058\000\041\000\041\000\037\000\038\000\018\000\
\054\000\093\000\019\000\034\000\005\000\006\000\061\000\007\000\
\039\000\028\000\072\000\017\000\029\000\066\000\067\000\107\000\
\027\000\070\000\110\000\030\000\073\000\089\000\033\000\040\000\
\114\000\035\000\043\000\036\000\055\000\082\000\060\000\008\000\
\009\000\010\000\011\000\012\000\013\000\084\000\020\000\021\000\
\085\000\059\000\071\000\091\000\088\000\022\000\023\000\086\000\
\111\000\095\000\094\000\106\000\096\000\097\000\092\000\098\000\
\099\000\100\000\024\000\112\000\025\000\101\000\108\000\102\000\
\103\000\104\000\020\000\021\000\116\000\105\000\109\000\115\000\
\044\000\022\000\023\000\113\000\122\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\024\000\117\000\
\025\000\119\000\118\000\120\000\121\000\002\000\000\000\023\000\
\123\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\020\000\083\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\090\000"

let yycheck = "\007\000\
\017\000\018\000\001\000\011\000\012\000\059\000\031\000\032\000\
\023\001\022\001\027\000\024\001\025\001\010\001\011\001\002\001\
\024\000\071\000\005\001\024\001\003\001\004\001\030\000\006\001\
\021\001\002\001\043\000\002\001\005\001\035\000\036\000\085\000\
\002\001\041\000\088\000\002\001\044\000\062\000\002\001\036\001\
\094\000\025\001\002\001\025\001\002\001\053\000\002\001\030\001\
\031\001\032\001\033\001\034\001\035\001\026\001\001\001\002\001\
\027\001\023\001\023\001\028\001\023\001\008\001\009\001\024\001\
\091\000\073\000\023\001\084\000\022\001\022\001\029\001\022\001\
\022\001\022\001\021\001\092\000\023\001\022\001\086\000\022\001\
\022\001\022\001\001\001\002\001\109\000\022\001\024\001\095\000\
\007\001\008\001\009\001\024\001\117\000\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\024\001\
\023\001\113\000\022\001\024\001\022\001\024\001\255\255\024\001\
\120\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\029\001\054\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\064\000"

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
  VAR\000\
  PROC\000\
  SET\000\
  IFPROG\000\
  WHILE\000\
  CALL\000\
  VOID\000\
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
# 31 "parser.mly"
                   ( AstProg(_2) )
# 253 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 35 "parser.mly"
                   ( AstStat(_1) )
# 260 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 36 "parser.mly"
                    ( AstDec(_1, _3) )
# 268 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 37 "parser.mly"
                    ( AstStats(_1, _3) )
# 276 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 41 "parser.mly"
                           ( AstEcho(_2) )
# 283 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 42 "parser.mly"
                           ( AstSet(AstId(_2), _3) )
# 291 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.prog) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.prog) in
    Obj.repr(
# 43 "parser.mly"
                           ( AstIF(_2, _3, _4) )
# 300 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.prog) in
    Obj.repr(
# 44 "parser.mly"
                           ( AstWhile(_2, _3) )
# 308 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 45 "parser.mly"
                           ( AstCall(AstId(_2), _3) )
# 316 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.type_) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                              ( AstConst(AstId(_2), _3, _4) )
# 325 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.type_) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                                              ( AstFun(AstId(_2), _3, _5, _7) )
# 335 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.type_) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                                              ( AstFunRec(AstId(_3), _4, _6, _8) )
# 345 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.type_) in
    Obj.repr(
# 52 "parser.mly"
                                              ( AstVar(AstId(_2), _3) )
# 353 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.prog) in
    Obj.repr(
# 53 "parser.mly"
                                              ( AstProc(AstId(_2), _4, _6) )
# 362 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.prog) in
    Obj.repr(
# 54 "parser.mly"
                                              ( AstProcRec(AstId(_3), _5, _7) )
# 371 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                                 ( AstTypeInt )
# 377 "parser.ml"
               : Ast.type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                                 ( AstTypeBool )
# 383 "parser.ml"
               : Ast.type_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.type_) in
    Obj.repr(
# 60 "parser.mly"
                                 ( AstTypeFun(_2, _4) )
# 391 "parser.ml"
               : Ast.type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                                 ( AstVoid )
# 397 "parser.ml"
               : Ast.type_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.type_) in
    Obj.repr(
# 65 "parser.mly"
                      ( AstType(_1) )
# 404 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.type_) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.types) in
    Obj.repr(
# 66 "parser.mly"
                       ( AstTypes(_1, _3) )
# 412 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.type_) in
    Obj.repr(
# 70 "parser.mly"
                      ( (AstId(_1), _3) )
# 420 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 74 "parser.mly"
                 ( AstArg(_1) )
# 427 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 75 "parser.mly"
                  ( AstArgs(_1, _3) )
# 435 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
                              ( AstNum(_1) )
# 442 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                               ( AstTrue )
# 448 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                               ( AstFalse )
# 454 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                               ( AstId(_1) )
# 461 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 82 "parser.mly"
                               ( AstPrim(Ast.Add, _3) )
# 468 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 83 "parser.mly"
                               ( AstPrim(Ast.Sub, _3) )
# 475 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 84 "parser.mly"
                               ( AstPrim(Ast.Mul, _3) )
# 482 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 85 "parser.mly"
                               ( AstPrim(Ast.Div, _3) )
# 489 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 86 "parser.mly"
                               ( AstPrim(Ast.And, _3) )
# 496 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 87 "parser.mly"
                               ( AstPrim(Ast.Or, _3) )
# 503 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 88 "parser.mly"
                               ( AstPrim(Ast.Eq, _3) )
# 510 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 89 "parser.mly"
                               ( AstPrim(Ast.Lt, _3) )
# 517 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 90 "parser.mly"
                               ( AstUnary(Ast.Not, _3) )
# 524 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 91 "parser.mly"
                               ( AstIf(_3, _4, _5) )
# 533 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 92 "parser.mly"
                               ( AstAbstraction(_2, _4) )
# 541 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 93 "parser.mly"
                               ( AstApply(_2, _3) )
# 549 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 97 "parser.mly"
                  ( [_1] )
# 556 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 98 "parser.mly"
                  ( _1::_2 )
# 564 "parser.ml"
               : Ast.expr list))
(* Entry prog *)
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
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
