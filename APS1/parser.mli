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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
