%{
open Ast
%}

%token <int> NUM
%token <string> IDENT
%token CONST FUN REC ECHO IF
%token TRUE FALSE
%token INT BOOL
%token PLUS MINUS TIMES DIV AND OR EQ LT NOT
%token LPAR RPAR LCRO RCRO
%token PNTV DPNT VIRG ETOILE FLECHE
%token VAR PROC SET IFPROG WHILE CALL VOID

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.prog> prog
%type <Ast.cmds> cmds
%type <Ast.stat> stat
%type <Ast.dec> dec
%type <Ast.type_> type_
%type <Ast.types> types
%type <Ast.arg> arg
%type <Ast.args> args


%start prog             /* the entry point */

%%
prog:
		LCRO cmds RCRO   { AstProg($2) }
;

cmds:
		stat             { AstStat($1) }
	| dec PNTV cmds    { AstDec($1, $3) }
	| stat PNTV cmds   { AstStats($1, $3) }
;

stat:
	ECHO expr                 { AstEcho($2) }
	| SET IDENT expr          { AstSet(AstId($2), $3) }
	| IFPROG expr prog prog   { AstIF($2, $3, $4) }
	| WHILE expr prog         { AstWhile($2, $3) }
	| CALL IDENT exprs        { AstCall(AstId($2), $3) }
;

dec:
		CONST IDENT type_ expr                      { AstConst(AstId($2), $3, $4) }
		| FUN IDENT type_ LCRO args RCRO expr       { AstFun(AstId($2), $3, $5, $7) }
		| FUN REC IDENT type_ LCRO args RCRO expr   { AstFunRec(AstId($3), $4, $6, $8) }
		| VAR IDENT type_                           { AstVar(AstId($2), $3) }
		| PROC IDENT LCRO args RCRO prog            { AstProc(AstId($2), $4, $6) }
		| PROC REC IDENT LCRO args RCRO prog        { AstProcRec(AstId($3), $5, $7) }
;

type_:
	INT                             { AstTypeInt }
	| BOOL                          { AstTypeBool }
	| LPAR types FLECHE type_ RPAR  { AstTypeFun($2, $4) }
	| VOID                          { AstVoid }
;

types:
		type_               { AstType($1) }
	| type_ ETOILE types  { AstTypes($1, $3) }
;

arg:
		IDENT DPNT type_    { (AstId($1), $3) }  /*NO NAME*/
;

args:
		arg            { AstArg($1) }
	|	arg VIRG args  { AstArgs($1, $3) }

expr:
		NUM                         { AstNum($1) }
	| TRUE                        { AstTrue }
	| FALSE                       { AstFalse }
	| IDENT                       { AstId($1) }
	| LPAR PLUS exprs RPAR        { AstPrim(Ast.Add, $3) }
	| LPAR MINUS exprs RPAR       { AstPrim(Ast.Sub, $3) }
	| LPAR TIMES exprs RPAR       { AstPrim(Ast.Mul, $3) }
	| LPAR DIV exprs RPAR         { AstPrim(Ast.Div, $3) }
	| LPAR AND exprs RPAR         { AstPrim(Ast.And, $3) }
	| LPAR OR exprs RPAR          { AstPrim(Ast.Or, $3) }
	| LPAR EQ exprs RPAR          { AstPrim(Ast.Eq, $3) }
	| LPAR LT exprs RPAR          { AstPrim(Ast.Lt, $3) }
	| LPAR NOT expr RPAR          { AstUnary(Ast.Not, $3) }
	| LPAR IF expr expr expr RPAR { AstIf($3, $4, $5) }
	| LCRO args RCRO expr         { AstAbstraction($2, $4) }
	| LPAR expr exprs RPAR        { AstApply($2, $3) }
;

exprs:
  expr            { [$1] }
| expr exprs      { $1::$2 }
;
