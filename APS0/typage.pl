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

/* PROG */
typeProg(prog(X)):- typeCmds([],X,void).


/* CMDS */
typeCmds(G,[stat(X)],void):- typeStat(G,X,void).
typeCmds(G,[stat(H)|CMDS],void):- typeStat(G,H,void),typeCmds(G,CMDS,void).
typeCmds(G,[dec(H)|CMDS],void):- typeDec(G,H,GG),typeCmds(GG,CMDS,void).


/* STAT */
typeStat(G,echo(E),void):- typeExpr(G,E,int). /* /!\ RETURN INT */


/* DEC */
typeDec(G,const(X,T,E),[(X,T)|G]):- typeExpr(G,E,T).

typeDec(G,fun(X, T, A, E), [(X,typefun(TS,T)) |G] ):-
    getTypes(A,TS), append(G,A,GG), typeExpr(GG,E,T).

typeDec(G,funrec(X,T,A,E),[(X,typefun(TS,T))|G]):- 
    getTypes(A,TS), append(G,A,NEWG1), append(NEWG1,[(X,typefun(TS,T))],NEWG2), typeExpr(NEWG2,E,T).



/* Verify type of args*/
getTypes([(_,T)],[T]).
getTypes([(_,T)|A], [T|TT]) :- getTypes(A,TT).

/* EXPR */
typeExpr(_, true, bool).
typeExpr(_, false, bool).
typeExpr(_, num(_), int).
typeExpr(G,var(X),T) :- sym((var(X),T),G).
/* OPERATIONS BOOLEAN */
typeExpr(G, not(E), bool) :- typeExpr(G, E, bool).
typeExpr(G, and(E1, E2), bool) :- typeExpr(G,E1,bool), typeExpr(G,E2,bool).
typeExpr(G, or(E1, E2), bool) :- typeExpr(G,E1,bool), typeExpr(G,E2,bool).
typeExpr(G, eq(E1, E2), bool) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
typeExpr(G, lt(E1, E2), bool) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
/* OPERATIONS INT */
typeExpr(G, add(E1, E2), int) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
typeExpr(G, sub(E1, E2), int) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
typeExpr(G, mul(E1, E2), int) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
typeExpr(G, div(E1, E2), int) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
/* ABS */
typeExpr(G, abst(A, E), typefun(TS,T)) :- getTypes(A,TS), append(G,A,GG), typeExpr(GG,E,T).
/* APP */
typeExpr(G, apply(E, ES),T) :- typeExpr(G, E, typefun(TS,T)), checkExprs(G, ES,TS).
/* EXPR SUITE */
typeExpr(G, if(COND,BODY,ALT),T) :- typeExpr(G, COND, bool), typeExpr(G, BODY, T), typeExpr(G, ALT, T).


/* Environnement: ident -> type */
%% Append
append([], X, X).
append([A|X], Y, [A|R]) :- append(X, Y, R).

%% Check Environment
sym((X,T),[(X,T)|_]). %%Trouvé
sym(X,[_|GS]) :- sym(X,GS).

%% Check Expressions of the APP
checkExprs(_,[],[]).
checkExprs(G,[E|ES], [TE|TES]) :- typeExpr(G,E,TE), checkExprs(G, ES, TES).
