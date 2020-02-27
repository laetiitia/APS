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
typeCmds(G,[dec(H)|CMDS],void):- typeDec(G,H,X),typeCmds(X,CMDS,void).


/* STAT */
typeStat(G,echo(E),void):- typeExpr(G,E,int). /* /!\ RETURN INT */


/* DEC */
typeDec(G,const(X,T,E),[(X,T)|G]):- typeExpr(G,E,T).

typeDec(G,fun(X, T, A, E), [(X,typefun(TS,T)) |G] ):-
    getTypes(A,TS), append(G,A,GG), typeExpr(GG,E,T).% getTypes(A,TS).

typeDec(G,funrec(X,T,A,E),[(X,typefun(TS,T))|G]):- 
    getTypes(A,TS), append(G,A,NEWG1), append(NEWG1,[(X,typefun(TS,T))],NEWG2), typeExpr(NEWG2,E,T).



/* Verify couple args-types*/
getTypes([arg(_,T)],[T]).
getTypes([arg(_,T)|A], [T|TT]) :- getTypes(A,TT).

/* EXPR */
typeExpr(_, true, bool).
typeExpr(_, false, bool).
typeExpr(_, num(_), int).
%typeExpr(G,var(X),T) :- check((X,T),G).
/* OPERATIONS BOOLEAN */
typeExpr(G, not(E), bool) :- typeExpr(G, E, bool).
typeExpr(G, and(E1, E2), bool) :- typeExpr(G,E1,bool), typeExpr(G,E2,bool).
typeExpr(G, or(E1, E2), bool) :- typeExpr(G,E1,bool), typeExpr(G,E2,bool).
typeExpr(G, eq(E1, E2), bool) :- typeExpr(G,E1,bool), typeExpr(G,E2,bool).
typeExpr(G, lt(E1, E2), bool) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
/* OPERATIONS INT */
typeExpr(G, add(E1, E2), int) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
typeExpr(G, sub(E1, E2), int) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
typeExpr(G, mult(E1, E2), int) :- typeExpr(G,E1,int), typeExpr(G,E2,int).
typeExpr(G, div(E1, E2), int) :- typeExpr(G,E1,int), typeExpr(G,E2,int).

typeExpr(G, if(COND,BODY,ALT),T) :- typeExpr(G, COND, bool), typeExpr(G, BODY, T), typeExpr(G, ALT, T).
typeExpr(G,abst(A, E), typefun(TS,T)) :- getTypes(A,TS), append(G,A,GG), typeExpr(GG,E,T).
%typeExpr(G,apply(E, ES),_) :- typeExpr(G,E,_), typeExprs(G,ES,_).
typeExpr(G,X,T) :- check((X,T),G).


/* Environnement: ident -> type */

%% Assoc

assoc(K, [(K,V)|_], V).
assoc(K, [_| KVs], V) :- assoc(K, KVs, V). 


%% Append

append([], X, X).
append([A|X], Y, [A|R]) :- append(X, Y, R).

%% Check Environment
check((X,T),[arg(X,T)|_]). %%Trouvé
check(X,[_|GS]) :- check(X,GS).


