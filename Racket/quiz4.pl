get_factors(N,L):-
    N2 is N-1,
    findall(X, (between(2,N2,X), N mod X =:= 0), L).

missing_values(L,M):-
    findall(X, (between(1,9,X), \+ member(X,L)), M).
