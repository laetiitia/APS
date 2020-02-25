main_stdin :-
    read(user_input,T),
    typeCheck(T,R),
    print(R),
    nl,
    exitCode(R).

/* CHECK*/
typeCheck(P,ok) :- typeProg(P).
typeCheck(_,ko).


/* EXIT */
exitCode(ok) :- halt(0).
exitCode(_) :- halt(1).
exitCode(_).


/* PROG */
typeProg(prog(X)):- typeCmds(_,X,void).


/* CMDS */
typeCmds(_,[stat(X)],void):- typeStat(_,X,void).
typeCmds(_,[stat(H)|CMDS],void):- typeStat(_,H,void),typeCmds(_,CMDS,void).
typeCmds(_,[dec(H)|CMDS],void):- typeDec(_,H,void),typeCmds(_,CMDS,void).


/* STAT */
typeStat(_,echo(E),void):- typeExpr(_,E,int). /* /!\ RETURN INT */


/* DEC */
typeDec(_,const(X,T,E),_):- typeExpr(_,X,_), typeType(T), typeExpr(_,E,T).
typeDec(_,fun(X,T,A,E),_):- typeExpr(_,X,_), typeType(T), typeArg(_,A,_), typeExpr(_,E,_).
typeDec(_,funrec(X,T,A,E),_):- typeExpr(_,X,_), typeType(T), typeArg(_,A,_), typeExpr(_,E,_).


/* ARG */
typeArg(_,[arg(X,T)],_):- typeExpr(_,X,string),typeType(T).
typeArg(_,[arg(X,T)|A],_):- typeExpr(_,X,string),typeType(T),typeArg(_,A,_).
 

/* TYPE */
typeType(bool).
typeType(int).
/*
typeType(_,typefun([X],type(Y)),_):- typeTypes(X), typeType(Y).
*/

/*TYPES*/



/* EXPR */
typeExpr(_, true, bool).
typeExpr(_, false, bool).
typeExpr(_, num(_),int).
typeExpr(_,var(_),string).
/* OPERATIONS BOOLEAN */
typeExpr(_, not(E), bool) :- typeExpr(_, E, bool).
typeExpr(_, and(E1, E2), bool) :- typeExpr(_,E1,bool), typeExpr(_,E2,bool).
typeExpr(_, or(E1, E2), bool) :- typeExpr(_,E1,bool), typeExpr(_,E2,bool).
typeExpr(_, eq(E1, E2), bool) :- typeExpr(_,E1,bool), typeExpr(_,E2,bool).
typeExpr(_, lt(E1, E2), bool) :- typeExpr(_,E1,int), typeExpr(_,E2,int).
/* OPERATIONS INT */
typeExpr(_, add(E1, E2), int) :- typeExpr(_,E1,int), typeExpr(_,E2,int).
typeExpr(_, sub(E1, E2), int) :- typeExpr(_,E1,int), typeExpr(_,E2,int).
typeExpr(_, mult(E1, E2), int) :- typeExpr(_,E1,int), typeExpr(_,E2,int).
typeExpr(_, div(E1, E2), int) :- typeExpr(_,E1,int), typeExpr(_,E2,int).

typeExpr(_, if(COND,BODY,ALT),T) :- typeExpr(_, COND, bool), typeExpr(_, BODY, T), typeExpr(_, ALT, T).
%%typeExpr(_,abst(A, E), T) :- .
%%typeExpr(_,apply(E, ES), T) :- typeExpr(_,E,_).

/* EXPRS*/


/* Environnement: ident -> type */

%% Assoc
assoc(K, [(K,V)|_], V).
assoc(K, [_| KVs], V) :- assoc(K, KVs, V).

%% Append
append([], X, X).
append([A|X], Y, [A|R]) :- append(X, Y, R).


