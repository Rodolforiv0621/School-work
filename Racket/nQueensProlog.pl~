nc(V,P,D):-
    P=[H|T],
    V =\= H,
    D =\= abs(V-H),
    DP1 is D+1,
    nc(V,T,DP1).

nc(_,[],_).

solve(N,QP):-
    solve(N,[],QP).

solve(N,QPT,QP):-
    length(QPT,L),
    L < N,
    NM1 is N-1,
    between(0,NM1,V),
    nc(V,QPT,1),
    QPT1 = [V|QPT],
    solve(N,QPT1,QP).

