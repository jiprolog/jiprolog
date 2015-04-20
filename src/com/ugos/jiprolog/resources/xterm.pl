/******************************************************************
 * Terms extension package
 *
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

:-module(jipxterms, [numbervars/3, free_variables/2, term_variables/2, copy_term/2, name/2,
                      char_code/2, atom_codes/2, atom_chars/2, number_codes/2,
                      atom_length/2, number_chars/2, atom_number/2, atom_concat/3,
                      concat_atom/2, concat_atom/3, upcase/1, downcase/1,
                      upcase_char/2, upcase_chars/2, downcase_char/1, downcase_chars/2,
                      upcase_atom/2, downcase_atom/2, string_to_atom/2, string_to_list/2,
                      string_length/2, string_concat/3, vars/2, sub_atom/5, subsumes_term/2, unify_with_occurs_check/2]).

:-'$custom_built_in'([numbervars/3, free_variables/2, term_variables/2, copy_term/2, name/2,
                      char_code/2, atom_codes/2, atom_chars/2, number_codes/2,
                      atom_length/2, number_chars/2, atom_number/2, atom_concat/3,
                      concat_atom/2, concat_atom/3, upcase/1, downcase/1,
                      upcase_char/2, upcase_chars/2, downcase_char/1, downcase_chars/2,
                      upcase_atom/2, downcase_atom/2, string_to_atom/2, string_to_list/2,
                      string_length/2, string_concat/3, vars/2, sub_atom/5, subsumes_term/2, unify_with_occurs_check/2]).

:-assert(ver(jipxterms, '4.0.1')).

vars(Term, Vars):-
    xcall('com.ugos.jiprolog.extensions.terms.Vars2', [Term, Vars]).

numbervars(Term, Start, End):-
    xcall('com.ugos.jiprolog.extensions.terms.Numbervars3', [Term, Start, End]).

free_variables(Term, VarList):-
    xcall('com.ugos.jiprolog.extensions.terms.FreeVariables2', [Term, VarList]).

term_variables(Term, VarList):-
    xcall('com.ugos.jiprolog.extensions.terms.FreeVariables2', [Term, VarList]).

copy_term(Term, Copy):-
    xcall('com.ugos.jiprolog.extensions.terms.CopyTerm2', [Term, Copy]).

name(Atom, CharList):-
    xcall('com.ugos.jiprolog.extensions.terms.Name2', [Atom, CharList]).


char_code(Char, Code) :-
	(	atom(Char), \+ length(Char, 1) ->
		error(type_error(character,Char))
	;	nonvar(Code), \+ integer(Code) ->
		error(type_error(integer,Code))
	;	xcall('com.ugos.jiprolog.extensions.terms.AtomCodes2', [Char, [Code]])
	).


atom_codes(Atom, Codes) :-
	check_atom_codes_2_codes(Codes, Atom),
	xcall('com.ugos.jiprolog.extensions.terms.AtomCodes2', [Atom, Codes]).

check_atom_codes_2_codes(Codes, _) :-
	var(Codes),
	!.
check_atom_codes_2_codes([Code| _], Atom) :-
	var(Code),
	!,
	(	var(Atom) ->
		error(instantiation_error)
	;	true
	).
check_atom_codes_2_codes([Code| Codes], Atom) :-
	integer(Code),
	!,
	check_atom_codes_2_codes(Codes, Atom).
check_atom_codes_2_codes([Code| _], _) :-
	!,
	error(type_error(integer,Code)).
check_atom_codes_2_codes([], _) :-
	!.
check_atom_codes_2_codes(Codes, _) :-
	error(type_error(list,Codes)).


atom_chars(Atom, Chars) :-
	check_atom_chars_2_chars(Chars, Atom),
	xcall('com.ugos.jiprolog.extensions.terms.AtomChars2', [Atom, Chars]).

check_atom_chars_2_chars(Chars, _) :-
	var(Chars),
	!.
check_atom_chars_2_chars([Char| _], Atom) :-
	var(Char),
	!,
	(	var(Atom) ->
		error(instantiation_error)
	;	true
	).
check_atom_chars_2_chars([Char| Chars], Atom) :-
	atom(Char),
	length(Char, 1),
	% a character
	!,
	check_atom_chars_2_chars(Chars, Atom).
check_atom_chars_2_chars([Char| _], _) :-
	!,
	error(type_error(character,Char)).
check_atom_chars_2_chars([], _) :-
	!.
check_atom_chars_2_chars(Chars, _) :-
	error(type_error(list,Chars)).


number_codes(Number, Codes) :-
	check_number_codes_2_codes(Codes, Number),
	xcall('com.ugos.jiprolog.extensions.terms.NumberCodes2', [Number, Codes]).

check_number_codes_2_codes([Code| _], Number) :-
	var(Code),
	!,
	(	var(Number) ->
		error(instantiation_error)
	;	true
	).
check_number_codes_2_codes([Code| Codes], Number) :-
	integer(Code),
	!,
	check_number_codes_2_codes(Codes, Number).
check_number_codes_2_codes([Code| _], _) :-
	!,
	error(type_error(integer,Code)).
check_number_codes_2_codes([], _) :-
	!.
check_number_codes_2_codes(Codes, _) :-
	error(type_error(list,Codes)).


number_chars(Number, Chars) :-
	check_number_chars_2_chars(Chars, Number),
	xcall('com.ugos.jiprolog.extensions.terms.NumberChars2', [Number, Chars]).

check_number_chars_2_chars([Char| _], Number) :-
	var(Char),
	!,
	(	var(Number) ->
		error(instantiation_error)
	;	true
	).
check_number_chars_2_chars([Char| Chars], Number) :-
	atom(Char),
	length(Char, 1),
	% a character
	!,
	check_number_chars_2_chars(Chars, Number).
check_number_chars_2_chars([Char| _], _) :-
	!,
	error(type_error(character,Char)).
check_number_chars_2_chars([], _) :-
	!.
check_number_chars_2_chars(Chars, _) :-
	error(type_error(list,Chars)).


atom_number(Atom, Number) :-
    atom_codes(Atom, Codes),
    catch(number_codes(Number, Codes), _, fail).


atom_concat(Atom1, Atom2, Concat):-
    (atom(Atom1) ; number(Atom1)),
    (atom(Atom2) ; number(Atom2)),
    !,
    atom_chars(Atom1, CAtom1),
    atom_chars(Atom2, CAtom2),
    append(CAtom1, CAtom2, CConcat),
    atom_chars(Concat, CConcat),
    !.

atom_concat(Atom1, Atom2, Concat):-
    (atom(Concat) ; number(Concat)),
    !,
    atom_chars(Concat, CConcat),
    append(CAtom1, CAtom2, CConcat),
    atom_chars(Atom1, CAtom1),
    atom_chars(Atom2, CAtom2).

atom_concat(Atom1, _, Concat):-
    var(Atom1), var(Concat),
    error(instantiation_error).

atom_concat(_, Atom2, Concat):-
    var(Atom2), var(Concat),
    error(instantiation_error).

atom_concat(Atom1, _, _):-
	nonvar(Atom1),
    \+ atom(Atom1),
    error(type_error(atom,Atom1)).

atom_concat(_, Atom2, _):-
	nonvar(Atom2),
    \+ atom(Atom2),
    error(type_error(atom,Atom2)).

atom_concat(_, _, Concat):-
    error(type_error(atom,Concat)).


sub_atom(Atom, Before, Length, After, SubAtom):-
 	atom(Atom), (var(SubAtom); atom(SubAtom)),
 	% the other error conditions are checked by the calls to atom_length/2
 	!,
 	atom_concat(Prefix, Suffix, Atom),
 	atom_concat(SubAtom, AfterAtom, Suffix),
 	atom_length(SubAtom, Length),
 	atom_length(Prefix, Before),
 	atom_length(AfterAtom, After).

sub_atom(Atom, _, _, _, _):-
 	var(Atom),
 	error(instantiation_error).

sub_atom(Atom, _, _, _, _):-
 	\+ atom(Atom),
 	error(type_error(atom, Atom)).

sub_atom(_, _, _, _, SubAtom):-
 	% nonvar(SubAtom),
 	% \+ atom(SubAtom),
 	error(type_error(atom, SubAtom)).

%sub_atom(Atom, Before, Length, After, SubAtom):-
%	atom_concat(Prefix, Suffix, Atom),
%	atom_concat(BeforeAtom, SubAtom, Prefix),
%	atom_length(SubAtom, Length),
%	atom_length(BeforeAtom, Before),
%	atom_length(Suffix, After).


concat_atom([A1], A1).
concat_atom([A1, A2|List], Atom):-
    atom_concat(A1, A2, C),
    concat_atom([C|List], Atom).

concat_atom([A1], Sep, A1).
concat_atom([A1, A2|List], Sep, Atom):-
    atom_concat(A1, Sep, C1),
    atom_concat(C1, A2, C),
    concat_atom([C|List], Sep, Atom).

atom_length(Atom, Length):-
 ( atom(Atom) ->
  length(Atom, Length)
 ; var(Atom) ->
  error(instantiation_error)
 ; error(type_error(atom,Atom))
 ).
%atom_length(Atom, Len):-
%    length(Atom, Len).

atom_prefix(Atom, Prefix):-
    atom_concat(Prefix, _, Atom).

upcase(Char):-
    Char >= 64,
    Char =< 95.

downcase(Char):-
    Char >= 96,
    Char =< 128.

upcase_char(Char, UChar):-
    upcase(Char),
    UChar = Char.

upcase_char(Char, UChar):-
    downcase(Char),
    UChar is Char - 32.

downcase_char(Char, LChar):-
    downcase(Char),
    UChar = Char.

downcase_char(Char, LChar):-
    upcase(Char),
    LChar is Char + 32.

upcase_chars([], []).

upcase_chars([Char|Rest], [UChars|URest]):-
    upcase_char(Char, UChars),
    upcase_chars(Rest, URest).

downcase_chars([], []).

downcase_chars([Char|Rest], [LChars|LRest]):-
    downcase_char(Char, LChars),
    downcase_chars(Rest, LRest).

downcase_atom(Atom, LAtom):-
    name(Atom ,CAtom),
    downcase_chars(CAtom, CLAtom),
    name(LAtom, CLAtom).

upcase_atom(Atom, UAtom):-
    name(Atom ,CAtom),
    upcase_chars(CAtom, CUAtom),
    name(UAtom, CUAtom).

string_to_atom(String, Atom):-
    name(Atom, String).

string_to_list(String, String):-
    string(String),
    !.

string_to_list(String, _):-
	error(type_error(atom, String)).

string_length(String, Len):-
    length(String, Len).

string_concat(String1, String2, Concat):-
    append(String1, String2, Concat).


subsumes_term(General, Specific) :-
	\+ \+ '$subsumes'(General, Specific).

'$subsumes'(General, Specific) :-
	term_variables(Specific, Vars1),
	unify_with_occurs_check(General, Specific),
	term_variables(Vars1, Vars2),
	Vars1 == Vars2.


var_member_chk(Var, [Head| Tail]) :-
	(	Var == Head ->
		true
	;	var_member_chk(Var, Tail)
	).

unify_with_occurs_check(Term1, Term2) :-
	Term1 = Term2,
	acyclic_term(Term1).


%*************************************
convert_chars([], []).

convert_chars([C|CharList], [A|AtomList]):-
    name(A, [C]),
    convert_chars(CharList, AtomList).

