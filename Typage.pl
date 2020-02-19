main_stdin :-
    read(user_input,T),
    typeCheck(T,R),
    print(R),
    nl,
    exitCode(R).

typeCheck(P,ok) :- typeProg(P).
typeCheck(_,ko).

exitCode(ok) :- halt(0).
exitCode(_) :- halt(1).
exitCode(_).

typeType(bool).
typeType(int).
typeType(string).

typeExpr(_, true, bool).
typeExpr(_, false, bool).
typeExpr(_, num(_),int).
typeExpr(_,var(_),string).

typeExpr(_, not(X), bool) :- typeExpr(_, X, bool).
typeExpr(_, and( X, Y), bool) :- typeExpr(_,X,bool),typeExpr(_,Y,bool).
typeExpr(_, or(X,Y), bool) :- typeExpr(_,X,bool),typeExpr(_,Y,bool).
typeExpr(_, eq(X,Y), bool):- typeExpr(_,X,Z),typeExpr(_,Y,Z).
typeExpr(_, lt(X,Y), bool):- typeExpr(_,X,int),typeExpr(_,Y,int).
typeExpr(_, add(X,Y), int):- typeExpr(_,X,int),typeExpr(_,Y,int).
typeExpr(_, sub(X,Y), int):- typeExpr(_,X,int),typeExpr(_,Y,int).
typeExpr(_, mult(X,Y), int):- typeExpr(_,X,int),typeExpr(_,Y,int).
typeExpr(_, div(X,Y), int):- typeExpr(_,X,int),typeExpr(_,Y,int).


typeProg(prog(X)):- typeCmds(_,X,void).


typeCmds(_,[stat(X)],void):- typeStat(_,X,void).
typeCmds(_,[stat(H)|T],void):- typeStat(_,H,void),typeCmds(_,T,void).
typeCmds(_,[dec(H)|T],void):- typeDec(_,H,void),typeCmds(_,T,void).

typeStat(_,X,void):- typeExpr(_,X,_).
typeDec(_,const(X,Y,Z),_):- typeExpr(_,X,_),typeType(Y),typeExpr(_,Z,_).
typeDec(_,fun(X,Y,Z,P),_):- typeExpr(_,X,_),typeType(Y),typeArg(_,Z,_),typeExpr(_,P,_).
typeDec(_,funrec(X,Y,Z,P),_):- typeExpr(_,X,_),typeType(Y),typeArg(_,Z,_),typeExpr(_,P,_).


typeArg(_,[arg(X,Y)],_):- typeExpr(_,X,string),typeType(Y).
typeArg(_,[arg(X,Y)|Z],_):- typeExpr(_,X,string),typeType(Y),typeArg(_,Z,_).


/* Environnement: ident -> type */

%% Assoc
assoc(K, [(K,V)|_], V).
assoc(K, [_| KVs], V) :- assoc(K, KVs, V).

%% Append
append([], X, X).
append([A|X], Y, [A|R]) :- append(X, Y, R).


