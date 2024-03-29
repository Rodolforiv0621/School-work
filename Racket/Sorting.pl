%Rodolfo Rivera
%------------------------------------
% Prolog Sorting Programming Project: selection sort and merge sort
%------------------------------------

%--------------------------------------
% support predicates rv/2, rl/3, is_sorted/1
%--------------------------------------
rv(R,V):-
    random(X),
    V is floor(X * R).

rl(N,R,L):-
    N>1,
    NM1 is N-1,
    rl(NM1,R,L1),
    rv(R,X),
    L = [X|L1].

rl(1,R,L):-
    rv(R,X),
    L = [X].

is_sorted([_]).
is_sorted(L) :-
    length(L,LL),
    LL > 1,
    L = [L1,L2|T],
    L1 =< L2,
    is_sorted([L2|T]).

%---------------------------------------
% Selection Sort Predicates: swap/4, min_index/3, selection_sort/2
%---------------------------------------

min_index(L,S,MI):-
    length(L, LL),
    SP is S+1,
    min_index(L,S,SP,LL,MI).

min_index(L,CM,C,E,MI):-
    nth0(CM,L,CMV),
    nth0(C,L,CV),
    CP is C+1,
    (   CV < CMV
    ->  min_index(L,C,CP,E,MI)
    ;   min_index(L,CM,CP,E,MI)
    ).
min_index(_,CM,E,E,CM).

swap(L,A,B,M):-
    nth0(A,L,AV),
    nth0(B,L,BV),
    nth0(A,L,_,R1),
    nth0(A,R2,BV,R1),
    nth0(B,R2,_,R3),
    nth0(B,M,AV,R3).

selection_sort(L,M):-
    length(L,LL),
    LLM is LL -2,
    selection_sort(L,0,LLM,M).

selection_sort(L,S,E,M):-
    S =< E,
    min_index(L,S,MI),
    (   S=\= MI
    ->  swap(L,S,MI,L1)
    ;   L1 = L
    ),
    SP is S + 1,
    selection_sort(L1,SP,E,M).
selection_sort(L,S,E,M):-
    S>E,
    M=L.

%---------------------------------------
% Merge Sort Predicates: split/3, merge/3, merge_sort/2
%----------------------------------------

split(L,L1,L2):-
    append(L1,L2,L),
    length(L,LL),
    LH is LL//2,
    length(L1,LH).

%merge(L1,L2,M):-
%    L1 = [L1H|L1T],
%    L2 = [L2H|L2T],
%    (   L1H =< L2H
%    ->  merge(L1T,L2,TEMP),M=[L1H|TEMP]
%    ;   merge(L1,L2T,TEMP),M=[L2H|TEMP]
%    ).

merge_sort([X],[X]).
merge_sort(L,M):-
    split(L,L1,L2),
    merge_sort(L1,L1S),
    merge_sort(L2,L2S),
    merge(L1S,L2S,M).
