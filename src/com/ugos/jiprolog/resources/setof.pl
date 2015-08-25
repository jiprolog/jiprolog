%   File   : setof.PL
%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: define setof/3, bagof/3, findall/3, and findall/4
%   Needs  : Not.Pl

/*  This file defines two predicates which act like setof/3 and bagof/3.
    I have seen the code for these routines in Dec-10 and in C-Prolog,
    but I no longer recall it, and this code was independently derived
    in 1982 by me and me alone.

    Most of the complication comes from trying to cope with free variables
    in the Filter; these definitions actually enumerate all the solutions,
    then group together those with the same bindings for the free variables.
    There must be a better way of doing this.  I do not claim any virtue for
    this code other than the virtue of working.  In fact there is a subtle
    bug: if setof/bagof occurs as a data structure in the Generator it will
    be mistaken for a call, and free variables treated wrongly.  Given the
    current nature of Prolog, there is no way of telling a call from a data
    structure, and since nested calls are FAR more likely than use as a
    data structure, we just put up with the latter being wrong.  The same
    applies to negation.

    Would anyone incorporating this in their Prolog system please credit
    both me and David Warren;  he thought up the definitions, and my
    implementation may owe more to subconscious memory of his than I like
    to think.  At least this ought to put a stop to fraudulent claims to
    having bagof, by replacing them with genuine claims.

    Thanks to Dave Bowen for pointing out an amazingly obscure bug: if
    the Template was a variable and the Generator never bound it at all
    you got a very strange answer!  Now fixed, at a price.
*/

:- module(jipxsetof,
    [ findall/3,		%   Same effect as C&M p152
	  findall/4,		%   A variant I have found very useful
	  bagof/3,			%   Like bagof (Dec-10 manual p52)
	  setof/3]).		%   Like setof (Dec-10 manual p51)

%   findall(Template, Generator, List)
%   is a special case of bagof, where all free variables in the
%   generator are taken to be existentially quantified.  It is
%   described in Clocksin & Mellish on p152.  The code they give
%   has a bug (which the Dec-10 bagof and setof predicates share)
%   which this has not.

%findall(Template, Generator, List) :-
%	save_instances(Template, Generator),
%	list_instances([], List).

%findall(Template, Generator, List, SoFar) :-
%	save_instances(Template, Generator),
%	list_instances(SoFar, List).


%   setof(Template, Generator, Set)
%   finds the Set of instances of the Template satisfying the Generator.
%   The set is in ascending order (see compare/3 for a definition of
%   this order) without duplicates, and is non-empty.  If there are
%   no solutions, setof fails.  setof may succeed more than one way,
%   binding free variables in the Generator to different values.  This
%   predicate is defined on p51 of the Dec-10 Prolog manual.

setof(Template, Filter, Set) :-
	bagof(Template, Filter, Bag),
	sort(Bag, Set0),
	Set = Set0.


%   bagof(Template, Generator, Bag)
%   finds all the instances of the Template produced by the Generator,
%   and returns them in the Bag in they order in which they were found.
%   If the Generator contains free variables which are not bound in the
%   Template, it assumes that this is like any other Prolog question
%   and that you want bindings for those variables.  (You can tell it
%   not to bother by using existential quantifiers.)
%   bagof records three things under the key '.':
%	the end-of-bag marker	       -
%	terms with no free variables   -Term
%	terms with free variables   Key-Term
%   The key '.' was chosen on the grounds that most people are unlikely
%   to realise that you can use it at all, another good key might be ''.
%   The original data base is restored after this call, so that setof
%   and bagof can be nested.  If the Generator smashes the data base
%   you are asking for trouble and will probably get it.
%   The second clause is basically just findall, which of course works in
%   the common case when there are no free variables.

bagof(Template, Generator, Bag) :-
	free_variables(Generator, Template, [], Vars, 0),
	Vars \== [],
	!,
	Key =.. [.|Vars],
	functor(Key, ., N),
	save_instances(Key-Template, Generator),
	list_instances(Key, N, [], OmniumGatherum),
%	write(OmniumGatherum), nl,
	keysort(OmniumGatherum, Gamut), !,
%	write(Gamut), nl,
	concordant_subset(Gamut, Key, Answer),
%	write(Answer), nl,
	Bag = Answer.

bagof(Template, Generator, Bag) :-
	findall(Template, Generator, Bag0),
	Bag0 \== [],
	Bag = Bag0.

%bagof(Template, Generator, Bag) :-
%	save_instances(Template, Generator),
%	list_instances([], Bag),
%	Bag \== [].



%   save_instances(Template, Generator)
%   enumerates all provable instances of the Generator and records the
%   associated Template instances.  Neither argument ends up changed.

save_instances(Template, Generator) :-
	recorda(-),
	call(Generator),
	recorda(Template),
	fail.

save_instances(_, _).


%   list_instances(SoFar, Total)
%   pulls all the -Template instances out of the data base until it
%   hits the - marker, and puts them on the front of the accumulator
%   SoFar.  This routine is used by findall/3-4 and by bagof when
%   the Generator has no free variables.

list_instances(SoFar, Total) :-
	recorded(Term),
	!,		%   must not backtrack
	list_instances(Term, SoFar, Total).


list_instances(Term, SoFar, Total) :-
	Term == (-),
	!,
	Total = SoFar.		%   = delayed in case Total was bound

list_instances(Template, SoFar, Total) :-
	list_instances([Template|SoFar], Total).



%   list_instances(Key, NVars, BagIn, BagOut)
%   pulls all the Key-Template instances out of the data base until
%   it hits the - marker.  The Generator should not touch recordx(.,_,_).
%   Note that asserting something into the data base and pulling it out
%   again renames all the variables; to counteract this we use replace_
%   key_variables to put the old variables back.  Fortunately if we
%   bind X=Y, the newer variable will be bound to the older, and the
%   original key variables are guaranteed to be older than the new ones.
%   This replacement must be done @i<before> the keysort.

list_instances(Key, NVars, OldBag, NewBag) :-
	recorded(Term),
	!,		%  must not backtrack!
	list_instances(Term, Key, NVars, OldBag, NewBag).


list_instances(Term, _, _, AnsBag, AnsBag) :-
	Term == (-),
	!.
list_instances(NewKey-Term, Key, NVars, OldBag, NewBag) :-
	replace_key_variables(NVars, Key, NewKey), !,
	list_instances(Key, NVars, [NewKey-Term|OldBag], NewBag).



%   There is a bug in the compiled version of arg in Dec-10 Prolog,
%   hence the rather strange code.  Only two calls on arg are needed
%   in Dec-10 interpreted Prolog or C-Prolog.

replace_key_variables(0, _, _) :- !.
replace_key_variables(N, OldKey, NewKey) :-
	arg(N, NewKey, Arg),
	nonvar(Arg), !,
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).

replace_key_variables(N, OldKey, NewKey) :-
	arg(N, OldKey, OldVar),
	arg(N, NewKey, OldVar),
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).



%   concordant_subset([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.

concordant_subset([Key-Val|Rest], Clavis, Answer) :-
	concordant_subset(Rest, Key, List, More),
	concordant_subset(More, Key, [Val|List], Clavis, Answer).


%   concordant_subset(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.

concordant_subset([Key-Val|Rest], Clavis, List, More) :-
%	write(key), nl, write(Key), nl,
%	write(clavis), nl, write(Clavis), nl,
	subsumes_term(Key, Clavis),
	subsumes_term(Clavis, Key),
	!,
	Key = Clavis,
	List = [Val|Rest2],
%	write(ok),
	concordant_subset(Rest, Clavis, Rest2, More).

concordant_subset(More, _, [], More).
%	write(nok).

%   concordant_subset/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choice point when this is
%   the last possible subset.

concordant_subset([],   Key, Subset, Key, Subset) :- !.
concordant_subset(_,    Key, Subset, Key, Subset).
concordant_subset(More, _,   _,   Clavis, Answer) :-
	concordant_subset(More, Clavis, Answer).


%   In order to handle variables properly, we have to find all the
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
%	a)  they occur in the template
%	b)  they are bound by X^P, setof, or bagof
%   free_variables(Generator, Template, OldList, NewList)
%   finds this set, using OldList as an accumulator.

free_variables(Term, Bound, VarList, [Term|VarList], _) :-
	var(Term),
	term_is_free_of(Bound, Term),
	list_is_free_of(VarList, Term),
	!.
free_variables(Term, _, VarList, VarList, _) :-
	var(Term),
	!.
free_variables(Term, Bound, OldList, NewList, 0) :-
	explicit_binding(Term, Bound, NewTerm, NewBound),
	!,
	free_variables(NewTerm, NewBound, OldList, NewList, 0).
free_variables(Term, Bound, OldList, NewList, _) :-
	functor(Term, _, N),
	free_variables(N, Term, Bound, OldList, NewList, 1).

free_variables(0, _, _, VarList, VarList, _) :- !.
free_variables(N, Term, Bound, OldList, NewList, Level) :-
	arg(N, Term, Argument),
	free_variables(Argument, Bound, OldList, MidList, Level),
	M is N-1, !,
	free_variables(M, Term, Bound, MidList, NewList, Level).

%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular \+ is quite common.

explicit_binding(\+ _,	       Bound, fail,	Bound      ) :- !.
explicit_binding(not(_),	   Bound, fail,	Bound	   ) :- !.
explicit_binding(Term^Goal,	   Bound, Goal,	Bound+Vars) :- !,
	term_variables(Term, Vars).
explicit_binding(setof(Var,Goal,Set), Bound, Goal-Set, Bound+Var) :- !.
explicit_binding(bagof(Var,Goal,Bag), Bound, Goal-Bag, Bound+Var) :- !.

term_is_free_of(Term, Var) :-
	var(Term), !,
	Term \== Var.
term_is_free_of(Term, Var) :-
	functor(Term, _, N),
	term_is_free_of(N, Term, Var).

term_is_free_of(0, _, _) :- !.
term_is_free_of(N, Term, Var) :-
	arg(N, Term, Argument),
	term_is_free_of(Argument, Var),
	M is N-1, !,
	term_is_free_of(M, Term, Var).


list_is_free_of([Head|Tail], Var) :-
	Head \== Var,
	!,
	list_is_free_of(Tail, Var).
list_is_free_of([], _).


:- dynamic('$bag'/1).

recorda(X) :-
	asserta('$bag'(X)).

recorded(Item):-
	retract('$bag'( Item )).
/*
recorded( SoFar, List ) :-
    retract('$bag'( Item )),
    !,
    /*  to stop retract looking for more Items.  */
    recorded( Item, SoFar, List ).


recorded(-, List, List ).

recorded(Template, SoFar, List ) :-
    recorded( [Template|SoFar], List ).

*/