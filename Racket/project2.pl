%Rodolfo Rivera
%
%ct1 and ct2 functions
%

ct1([],_,0).

ct1(L,X,C):-
    L=[H|T],
    ct1(T,X,CT),
    ( X==H
    -> C is CT+1
    ; C is CT
    ).

ct2(L,X,C):-
    findall(Y,(member(Y,L), Y==X),T),
    length(T,C).

%
%lg1 and lg2 functions
%
lg1(A,B,L):-
    A=B,
    L=[B].
lg1(B,B,[B]).
lg1(A,B,L):-
    A<B,
    LG1 is A+1,
    lg1(LG1,B,L1),
    L = [A|L1].
lg2(A,B,L):-
    findall(X, between(A,B,X), L).

