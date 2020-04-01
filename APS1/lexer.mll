{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}
rule token = parse
[' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
| ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
| "CONST"   { CONST }
| "FUN"     { FUN }
| "REC"     { REC }
| "ECHO"    { ECHO }
| "bool"    { BOOL }
| "int"     { INT }
| "true"    { TRUE }
| "false"	  { FALSE }
| "add"     { PLUS }
| "sub"     { MINUS }
| "mul"     { TIMES }
| "div"     { DIV }
| "and"		  { AND }
| "or"      { OR }
| "eq"		  { EQ }
| "lt"		  { LT }
| "not"     { NOT }
| "if"      { IF }
| "VAR"     { VAR }
| "PROC"    { PROC }
| "SET"     { SET }
| "IF"      { IFPROG }
| "WHILE"   { WHILE }
| "CALL"    { CALL }
| "void"    { VOID }
| '('              { LPAR }
| ')'              { RPAR }
| '['              { LCRO }
| ']'              { RCRO }
| ';'              { PNTV }
| ':'              { DPNT }
| ','              { VIRG }
| '*'              { ETOILE }
| "->"             { FLECHE }
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as lxm { IDENT(lxm) }
| eof              { raise Eof }
