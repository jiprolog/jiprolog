/******************************************************************
 * 24/04/2015
 *
 * Copyright (C) 2002 Ugo Chirico
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


:- module(jipflgs, [
	current_prolog_flag/2,
	set_prolog_flag/2,
	create_prolog_flag/3
]).

:- '$custom_built_in'([
	current_prolog_flag/2,
	set_prolog_flag/2,
	create_prolog_flag/3
]).

:- assert(ver(jipflgs, '1.0.0')).


% User-defined flags support predicates
:- dynamic(user_defined_flag/3).
%:- dynamic(user_defined_flag_value/2).

% ISO flags
prolog_flag(bounded, true).
prolog_flag(max_integer, Max) :- integer_bounds(_, Max).
prolog_flag(min_integer, Min) :- integer_bounds(Min, _).
prolog_flag(integer_rounding_function, toward_zero).
prolog_flag(max_arity, unbounded).
% Prolog Commons flags
prolog_flag(dialect, jiprolog).
prolog_flag(version_data, V) :-
	prolog_flag(dialect, X),
	ver(Major, Minor, Build, Revision),
	V =.. [X, Major, Minor, Build, Revision].
% Other flags
prolog_flag(prolog_name, 'JIProlog').
prolog_flag(prolog_version, Ver) :- ver(Ver).
prolog_flag(version, Ver) :- ver(Ver).
prolog_flag(prolog_copyright, C) :- copyright(C).
prolog_flag(pid, P) :- pid(P).
prolog_flag(encoding, E) :- encoding(E).

% ISO flags
valid_flag(bounded).
valid_flag(max_integer).
valid_flag(min_integer).
valid_flag(integer_rounding_function).
valid_flag(max_arity).
valid_flag(double_quotes).
valid_flag(char_conversion).
valid_flag(debug).
valid_flag(unknown).
% Prolog Commons flags
valid_flag(dialect).
valid_flag(version_data).
% Other flags
valid_flag(prolog_name).
valid_flag(prolog_version).
valid_flag(version).
valid_flag(prolog_copyright).
valid_flag(pid).
valid_flag(encoding).
valid_flag(enable_clause_check).
valid_flag(update_semantics).


read_only_flag(Flag) :-
	prolog_flag(Flag, _).

valid_flag_value(char_conversion, on) :- !.
valid_flag_value(char_conversion, off) :- !.
valid_flag_value(debug, on) :- !.
valid_flag_value(debug, off) :- !.
valid_flag_value(double_quotes, atom) :- !.
valid_flag_value(double_quotes, chars) :- !.
valid_flag_value(double_quotes, codes) :- !.
valid_flag_value(unknown, error) :- !.
valid_flag_value(unknown, warning) :- !.
valid_flag_value(unknown, fail) :- !.
valid_flag_value(enable_clause_check, true) :- !.
valid_flag_value(enable_clause_check, false) :- !.
valid_flag_value(update_semantics, logical) :- !.
valid_flag_value(update_semantics, immediate) :- !.

current_prolog_flag(Flag, Value) :-
	prolog_flag(Flag, Value).
%current_prolog_flag(Flag, Value) :-
%	user_defined_flag_value(Flag, Value).
current_prolog_flag(Flag, Value) :-
	env(Flag, Value).
current_prolog_flag(Flag, _) :-
	nonvar(Flag),
	\+ atom(Flag),
	error(type_error(atom,Flag)).
current_prolog_flag(Flag, _) :-
	nonvar(Flag),
	\+ valid_flag(Flag),
	\+ user_defined_flag(Flag, _, _),
	error(domain_error(prolog_flag,Flag)).

set_prolog_flag(Flag, _) :-
	var(Flag),
	error(instantiation_error).
set_prolog_flag(Flag, _) :-
	\+ atom(Flag),
	error(type_error(atom,Flag)).
set_prolog_flag(Flag, _) :-
	\+ valid_flag(Flag),
	\+ user_defined_flag(Flag, _, _),
	error(domain_error(prolog_flag,Flag)).
set_prolog_flag(Flag, _) :-
	(	read_only_flag(Flag)
	;	user_defined_flag(Flag, read_only, _)
	),
	error(permission_error(modify,flag,Flag)).
set_prolog_flag(Flag, Value) :-
	\+ (	valid_flag_value(Flag, Value)
		;	user_defined_flag(Flag, _, Type),
			call(Type, Value)
	),
	error(domain_error(flag_value,Flag+Value)).
set_prolog_flag(Flag, Value) :-
	set_env(Flag, Value).

create_prolog_flag(Flag, _, _) :-
	var(Flag),
	error(instantiation_error).
create_prolog_flag(_, Value, _) :-
	\+ ground(Value),
	error(instantiation_error).
create_prolog_flag(_, _, Options) :-
	\+ ground(Options),
	error(instantiation_error).
create_prolog_flag(Flag, _, _) :-
	\+ atom(Flag),
	error(type_error(atom,Flag)).
create_prolog_flag(_, _, Options) :-
	\+ is_list(Options),
	error(type_error(list,Options)).
create_prolog_flag(Flag, _, Options) :-
	valid_flag(Flag),
	error(permission_error(modify,flag,Flag)).
create_prolog_flag(_, _, Options) :-
	member(Option, Options),
	Option \= access(_),
	Option \= keep(_),
	Option \= type(_),
	error(domain_error(flag_option,Option)).
create_prolog_flag(_, _, Options) :-
	member(access(Access), Options),
	Access \== read_write,
	Access \== read_only,
	error(domain_error(flag_option,access(Access))).
create_prolog_flag(_, _, Options) :-
	member(keep(Keep), Options),
	Keep \== true,
	Keep \== false,
	error(domain_error(flag_option,keep(Keep))).
create_prolog_flag(_, Value, Options) :-
	member(type(Type0), Options),
	(	map_user_defined_flag_type(Type0, Type) ->
		(	call(Type, Value) ->
			fail
		;	error(type_error(Type0,Value))
		)
	;	error(domain_error(flag_option,type(Type0)))
	).
create_prolog_flag(Flag, _, Options) :-
	user_defined_flag(Flag, _, _),
	member(keep(true), Options),
	!.
create_prolog_flag(Flag, Value, Options) :-
	(	member(access(Access), Options) ->
		true
	;	Access = read_write
	),
	(	member(type(Type0), Options) ->
		map_user_defined_flag_type(Type0, Type)
	;	Value == true ->
		Type = is_boolean
	;	Value == false ->
		Type = is_boolean
	;	atom(Value) ->
		Type = atom
	;	integer(Value) ->
		Type = integer
	;	float(Value) ->
		Type = float
	;	% catchall
		Type = ground
	),
	retractall(user_defined_flag(Flag, _, _)),
	assertz(user_defined_flag(Flag, Access, Type)),
	set_env(Flag, Value).

map_user_defined_flag_type(boolean, is_boolean).
map_user_defined_flag_type(atom, atom).
map_user_defined_flag_type(integer, integer).
map_user_defined_flag_type(float, float).
map_user_defined_flag_type(term, ground).

is_boolean(true).
is_boolean(false).

/*
bounded(X):-prolog_flag(bounded, X).
min_integer(X):-prolog_flag(min_integer, X).
max_integer(X):-prolog_flag(max_integer, X).
integer_rounding_function(X):-prolog_flag(integer_rounding_function, X).
max_arity(X):-prolog_flag(max_arity, X).
max_arity(X):-prolog_flag(max_arity, X).
dialect(X):-prolog_flag(dialect, X).
*/
