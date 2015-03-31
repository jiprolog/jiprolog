/******************************************************************
 * JIPXSets
 * Copyright (C) 2002-2004 Ugo Chirico
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

:-module(jipxsets,
    [ is_set/1,
      list_to_set/2,
      intersection/3,
      union/3,
      subset/2,
      difference/3,
      symdiff/3,
      bsort/3,
      qsort/3,
      msort/3,
      sort/2,
      merge/2,
      merge_set/2,
      remove_duplicates/2,
      findall/3,
      findall/4,
      setof/3,
      bagof/3]).

:-'$custom_built_in'([
      sort/2,
      findall/3,
      findall/4,
      setof/3,
      bagof/3]).


:-module_transparent
      findall/3,
      findall/4,
      setof/3,
      bagof/3.

:-assert(ver(jipxsets, '3.0.1')).

%:-use_module('list.jip').

%   is_set(+Set)
is_set(0) :- !, fail.       % catch variables
is_set([]) :- !.
is_set([H|T]) :-
    memberchk(H, T), !,
    fail.
is_set([_|T]) :-
    is_set(T).

%   list_to_set(+List, ?Set)
list_to_set(List, Set) :-
    list_to_set_(List, Set0),
    Set = Set0.
list_to_set_([], R) :-
    is_list(R).
list_to_set_([H|T], R) :-
    memberchk(H, R), !,
    list_to_set_(T, R).

% intersection(+Set1, +Set2, -Set3)
intersection([],List,[]).   /* empty set intersect anything is empty */
intersection([Elemnt|List],List1,[Elemnt|List2]):-
      member(Elemnt,List1), /* Is Elemnt in List1 too? */
      !,
      intersection(List,List1,List2).
intersection([_|List],List1,List2):-
      intersection(List,List1,List2).

% union(+Set1, +Set2, -Set3)
union([],List,List).
union([Elemnt|List],List1,List2):-
      member(Elemnt,List1),
      !,
      union(List,List1,List2).
union([Elemnt|List],List1,[Elemnt|List2]):-
      union(List,List1,List2).

% subset(+Set1, ?Set2)
subset([],_).   /* empty set is in every set */
subset([Elemnt|List],List1):-
    member(Elemnt,List1),
    !,  /* don't let member be resatisfied */
    subset(List,List1).

% difference(+Set1, +Set2, -Set3)
difference([],_,[]):- !.
difference([Head|Rest],List2,[Head|Result]):-
          not member(Head,List2),
          difference(Rest,List2,Result).
difference([_|Rest],List2,Result):-
          difference(Rest,List2,Result).

% symdiff(+Set1, +Set2, -Set3)
symdiff(A,B,C) :-
    difference(A,B,A1),
    difference(B,A,B1),
    append(A1,B1,C).        /* why append not union? */

/* bubble sort */
bsort(Order,List,Sorted) :-
    append(List1,[I,II|List2],List),
    order(Order,II,I),
    append(List1,[II,I|List2],Newlist),
    !,
    bsort(Order,Newlist,Sorted).
bsort(Order,List,List).

/* quick sort */
qsort(Order,[Head|Rest],Result):-
     split(Order,Head,Rest,Gtr_than_Head,Smlr_than_head),
     qsort(Order,Gtr_than_Head,Grts),
     qsort(Order,Smlr_than_head,Smls),
     append(Grts,[Head|Smls],Result),!.
qsort(_,[],[]):- !.

split(Order,Marker,[First|Rest],[First|Grtr],Smlr):-
     order(Order,First,Marker),
     split(Order,Marker,Rest,Grtr,Smlr),!.

split(Order,Marker,[First|Rest],Grtr,[First|Smlr]):-
     order(Order,Marker,First),
     split(Order,Marker,Rest,Grtr,Smlr),!.

split(_,_,[],[],[]):- !.

order(Order,A,B) :-
    O =.. [Order,A,B],
    O.

/* merge sort */
msort(Rel, L,S ):-
    length(L,N),
    msort1(N, Rel, L,S,[] ).

msort1( 0,_,L,[],L ):-!.
msort1( 1,_,[X|L],[X],L ):-!.
msort1( N,Rel,L,S,R ):-             % N >= 2
    N1 is N >> 1,
    N2 is N-N1,
    msort1( N1,Rel,L,S1,R1),
    msort1( N2,Rel,R1,S2,R),
    merge2( S2,S1,Rel,S ).

merge2([],S,_,S ):-!.
merge2([X|L1],[Y|L2],Rel, [X|L] ):-
    order(Rel, X,Y),
    !,
    merge2(L1,[Y|L2],Rel, L ).

merge2(L1,[Y|L2],Rel, [Y|L] ):-
    merge2(L2,L1,Rel, L ).

% sort(?List, ?Sorted)
sort(L1,L2):-
    qsort(@=<,L1,DupL),
    remove_duplicates(DupL,L2),
    !.

/* merge two sets */
merge(L1, L2, L3):-
    merge2(@=<, L1, L2, L3).

merge_set(L1, L2, L3):-
    merge2(@=<, L1, L2, L31),
    remove_duplicates(L31, L3).

/* remove duplicates from a set */
remove_duplicates([], []) :-
 !.

remove_duplicates([X, Y| Xs], Ys) :-
 !,
 ( X == Y ->
  remove_duplicates([Y| Xs], Ys)
 ; Ys = [X| Ys2],
  remove_duplicates([Y|Xs], Ys2)
 ).

remove_duplicates([Y], [Y]).

findall(X, Goal, List):-
   xcall('com.ugos.jiprolog.extensions.sets.Findall3', [X, Goal, List]).

bagof(X, Goal, List1):-
    xcall('com.ugos.jiprolog.extensions.sets.Bagof3', [X, Goal, List1]),
    remove_duplicates(List1, List).

setof(X, Goal, List):-
    xcall('com.ugos.jiprolog.extensions.sets.Bagof3', [X, Goal, List1]),
    sort(List1, List).

findall(Term, Goal, List, Tail) :-
	findall(Term, Goal, List0),
	append(List0, Tail, List).
