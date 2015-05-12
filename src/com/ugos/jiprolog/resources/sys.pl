/******************************************************************
 * Sys extension package
 *
 * Copyright (C) 2002-2014 Ugo Chirico
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *******************************************************************/

:- module(jipsys,[
	meta_predicate/1,
    load_files/1,
    abolish_files/1,
	listing/1,
	listing/0,
	current_predicate/2,
	repeat/0,
	repeat/1,
	checklist/2,
	maplist/3,
	sublist/3,
	forall/2]).

:- module_transparent
    checklist/2,
    maplist/3,
    sublist/3,
    forall/2.

:-'$custom_built_in'([
	meta_predicate/1,
    load_files/1,
    abolish_files/1,
	listing/1,
	listing/0,
	current_predicate/2,
	repeat/0,
	repeat/1,
  	checklist/2,
  	maplist/3,
  	sublist/3,
  	forall/2]).

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

