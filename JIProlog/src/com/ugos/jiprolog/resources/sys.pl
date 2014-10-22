:- module(jipsys,[ 
	meta_predicate/1,
    load_files/1,
    abolish_files/1,
	listing/1,
	listing/0,			
	current_predicate/2,
	repeat/0,
	repeat/1, 
	one/1,
	once/1,
	checklist/2,
	maplist/3,
	sublist/3,
	forall/2,
	apply/2,
	call/2,
	call/3,
	call/4,
	call/5,
	ignore/1]).

:- module_transparent
    one/1,
    once/1,
    checklist/2,
    maplist/3,
    sublist/3,
    forall/2,
    apply/2,
    call/2,
    call/3,
    call/4,
    call/5,
    ignore/1.
    
:-'$custom_built_in'([
	meta_predicate/1,
    load_files/1,
    abolish_files/1,
	listing/1,
	listing/0,			
	current_predicate/2,
	repeat/0,
	repeat/1, 
  	one/1,
  	once/1,
  	checklist/2,
  	maplist/3,
  	sublist/3,
  	forall/2,
  	apply/2,
  	call/2,
  	call/3,
  	call/4,
  	call/5,
  	ignore/1]).

% Quintus / SWI compatibility    
:-op(1150, fx, meta_predicate).

meta_predicate(X) :- module_transparent X.


load_files([]):-!.
load_files([X|Rest]):-
    load(X),
    load_files(Rest).    
load_files(X):-
    load(X).


abolish_files([]):-!.
abolish_files([FileSpec|Rest]):-
    unconsult(FileSpec),
    abolish_files(Rest).    
abolish_files(FileSpec):-
    unconsult(FileSpec).

    
listing(Name/Arity):-
    current_functor(Name, Arity),
    write(Name), write('/'), write(Arity), nl,
    fail.    
listing(_).

listing:-listing(_).
    
 
current_predicate(Name, Head):-
	current_functor(Name, Arity),
	functor(Head, Name, Arity).
		
% repeat/1
repeat(N):-
    N > 0.
    
repeat(N):-
   N > 0,
   N1 is N - 1,
   repeat(N1).
    
%repeat/0
repeat.
repeat:-
    repeat.
     
%one/1
one(X):-X, !.

%once/1
once(X):-X, !.

%ignore/1
ignore(X):-X.
ignore(X):-!.

%apply
apply(Term,Args):-
    Term =.. L1,
    append(L1,Args,L2),
    Goal =.. L2,
        !,
    Goal.

% call/2,3,4,5
call(Closure,X1):-apply(Closure,[X1]).
call(Closure,X1,X2):-apply(Closure,[X1,X2]).
call(Closure,X1,X2,X3):-apply(Closure,[X1,X2,X3]).
call(Closure,X1,X2,X3,X4):-apply(Closure,[X1,X2,X3,X4]).

% checklist(+Goal, +List)
checklist(_G, []).
checklist(G, [E|T]) :-
    call(G, E),
    checklist(G, T).

% maplist(+Goal, +List1, ?List2)
maplist(_G, [], []).
maplist(G, [E1|T1], [E2|T2]) :-
    call(G, E1, E2),
    maplist(G, T1, T2).

%sublist(+Goal, +List1, ?List2)
sublist(_, [], []) :- !.
sublist(G, [H|T], S) :-
    call(G, H), !,
    S = [H|R],
    sublist(G, T, R).
        
sublist(G, [_|T], R) :-
    sublist(G, T, R).

%forall(+Condition, +Action)
forall(C, A) :-
    \+ (C, \+ A).
        
%*************************************

       