/*********************************************************************
 * JIProlog predicates related to lists
 *
 * 02/02/2015
 *
 * Copyright (C) 2002-2015 by Ugo Chirico. All Rights Reserved
 *
 ********************************************************************/

:- module(jipxlist,
    [ memberchk/2,
      select/3,
      nextto/3,
      delete/3,
      nth0/3,
      nth1/3,
      last/2,
      reverse/2,
      permutation/2,
      flatten/2,
      sumlist/2,
      numlist/3,
      sublist/2]).


%   memberchk(?Elem, ?List)
memberchk(X, L):-
    member(X, L),
    !.

%   select(?Elem, ?List1, ?List2)
select(X, [X|Tail], Tail).
select(Elem, [Head|Tail], [Head|Rest]) :-
    select(Elem, Tail, Rest).

%   nextto(?Elem, ?List1, ?List)
nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|Zs]) :-
    nextto(X, Y, Zs).

%   delete(?Elem, ?List1, ?List)
delete([], _, []) :- !.
delete([Elem|Tail], Elem, Result) :- !,
    delete(Tail, Elem, Result).
delete([Head|Tail], Elem, [Head|Rest]) :-
    delete(Tail, Elem, Rest).



%%  nth0(?Index, ?List, ?Elem)
nth0(Index, List, Elem) :-
    member(Elem, List, Index1),
    Index is Index1 - 1.

%%  nth1(?Index, ?List, ?Elem)
nth1(Index1, List, Elem) :-
    member(Elem, List, Index).

%   last(?List, ?Elem)
last([X|Xs], Last) :-
    last_(Xs, X, Last).

last_([], Last, Last).
last_([X|Xs], _, Last) :-
    last_(Xs, X, Last).

%   reverse(?List1, ?List2)
reverse(Xs, Ys) :-
    reverse(Xs, [], Ys, Ys).

reverse([], Ys, Ys, []).
reverse([X|Xs], Rs, Ys, [_|Bound]) :-
    reverse(Xs, [X|Rs], Ys, Bound).

%   premutation(?Xs, ?Ys)
permutation(Xs, Ys) :-
    permutation(Xs, Ys, Ys).

permutation([], [], []).
permutation([X|Xs], Ys1, [_|Bound]) :-
    permutation(Xs, Ys, Bound),
    select(X, Ys1, Ys).

%   flatten(+List1, ?List2)
flatten([],[]).

flatten([H|T],[H|T2]):-
	\+ is_list(H),
    flatten(T,T2).

flatten([H|T],L):-
	is_list(H),
    flatten(H,A),
    flatten(T,B),
	append(A,B,L).

%   sumlist(+List, -Sum)
sumlist(Xs, Sum) :-
    sumlist(Xs, 0, Sum).

sumlist([], Sum, Sum).
sumlist([X|Xs], Sum0, Sum) :-
    Sum1 is Sum0 + X,
    sumlist(Xs, Sum1, Sum).

%   numlist(+Low, +High, -List)
numlist(L, U, Ns) :-
    integer(L), integer(U), L =< U,
    numlist_(L, U, Ns).

numlist_(L, U, [L|Ns]) :-
    (
        L =:= U
        ->  Ns = []
        ;   M is L + 1,
        numlist_(M, U, Ns)
    ).

sublist( [], _ ).
sublist( [X|XS], [X|XSS] ) :- sublist( XS, XSS ).
sublist( [X|XS], [_|XSS] ) :- sublist( [X|XS], XSS ).
