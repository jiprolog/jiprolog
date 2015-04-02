/*************************
 * IO library
 *
 * Copyright (C) 1999-2004 Ugo Chirico
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
 **************************/

:-module(jipxio, [see/1, see/2, seen/0, seen/1, seeing/1, seeing/2, read/1, read/2, read_term/2, read_term/3, read_clause/1, read_clause/2, get0/1,
                  get0/2, get/1, get/2, get_byte/1, get_byte/2, get_code/1, get_code/2, get_char/1, get_char/2, peek_byte/1, peek_byte/2, peek_code/1,
                  peek_code/2, peek_char/1, peek_char/2, tell/1, tell/2, told/0, told/1, telling/1, telling/2, write/2, writeq/1,
                  writeq/2, writeln/1, writeln/2, write_canonical/2, write_term/2, write_term/3, put/1, put/2, put_byte/1, put_byte/2, put_code/1,
                  put_code/2, put_char/1, skip/1, skip/2, put_char/2, nl/1, flush_output/0, flush_output/1, open/3, open/4, close/1,
                  print/1, print/2, display/1, display/2, close/2, at_end_of_stream/0, at_end_of_stream/1, stream_property/2,
                  access_file/2, exists_file/1, exists_directory/1, same_file/2, working_directory/2, searchpath/1, absolute_file_name/2,
                  is_absolute_file_name/1, file_attributes/7, file_name_extension/2, size_file/2, time_file/2, file_directory_name/2,
                  file_base_name/2, delete_file/1, delete_directory/1, rename_file/2, dir/0, dir/1, make_directory/1, current_stream/3, cd/1,
                  current_output/1, current_input/1, current_stream/1, set_output/1, set_input/1, seek/4, set_stream_position/2, set_stream/2]).

:-'$custom_built_in'([see/1, see/2, seen/0, seen/1, seeing/1, seeing/2, read/1, read/2, read_term/2, read_term/3, read_clause/1, read_clause/2, get0/1,
                  get0/2, get/1, get/2, get_byte/1, get_byte/2, get_code/1, get_code/2, get_char/1, get_char/2, peek_byte/1, peek_byte/2, peek_code/1,
                  peek_code/2, peek_char/1, peek_char/2, tell/1, tell/2, told/0, told/1, telling/1, telling/2, write/2, writeq/1,
                  writeq/2, writeln/1, writeln/2, write_canonical/2, write_term/2, write_term/3, put/1, put/2, put_byte/1, put_byte/2, put_code/1,
                  put_code/2, put_char/1, skip/1, skip/2, put_char/2, nl/1, flush_output/0, flush_output/1, open/3, open/4, close/1,
                  print/1, print/2, display/1, display/2, close/2, at_end_of_stream/0, at_end_of_stream/1, stream_property/2,
                  access_file/2, exists_file/1, exists_directory/1, same_file/2, working_directory/2, searchpath/1, absolute_file_name/2,
                  is_absolute_file_name/1, file_attributes/7, file_name_extension/2, size_file/2, time_file/2, file_directory_name/2,
                  file_base_name/2, delete_file/1, delete_directory/1, rename_file/2, dir/0, dir/1, make_directory/1, current_stream/3, cd/1,
                  current_output/1, current_input/1, current_stream/1, set_output/1, set_input/1, seek/4, set_stream_position/2, set_stream/2]).

:-assert(ver(jipxio, '4.0.2')).

:-op(400, fx, cd).


open(File, Mode, _, Options):-
	(	var(File)
	;	var(Mode)
	;	var(Options)
	),
	error(instantiation_error).

open(_, _, _, Options):-
	member(Option, Options),
	\+ ground(Option),
	error(instantiation_error).

open(_, Mode, _, _):-
	\+ atom(Mode),
	error(type_error(atom,Mode)).

open(_, _, _, Options):-
	\+ is_list(Options),
	error(type_error(list,Options)).

open(_, _, Handle, _):-
	nonvar(Handle),
%	error(uninstantiation_error(Options)).
	error(type_error(variable,Options)).

open(File, _, _, _):-
	\+ atom(File),
	error(domain_error(source_sink,File)).

open(_, Mode, _, _):-
	Mode \== read,
	Mode \== write,
	Mode \== append,
	error(domain_error(io_mode,Mode)).

open(_, _, _, Options):-
	member(alias(Alias), Options),
	stream_property(_, alias(Alias)),
	error(permission_error(open,source_sink,alias(Alias))).

open(File, Mode, Handle, Options):-
    open_(File, Mode, Handle),
    set_stream_properties(Handle, Options).


open(File, Mode, Handle):-
	open(File, Mode, Handle, []).


open_(File, write, Handle):-
    tell(File, Handle).

open_(File, read, Handle):-
    see(File, Handle).

open_(File, append, Handle):-
    append(File, Handle).


current_stream(FileName, Mode, Handle):-
	nonvar(Handle),
	!,
	check_handle(Handle, Handle1),
    stream_property(Handle1, file_name(FileName)),
    stream_property(Handle1, mode(Mode)).

current_stream(FileName, Mode, Handle):-
    stream_property(Handle, file_name(FileName)),
    stream_property(Handle, mode(Mode)).

/**********************************
* Read Predicates
***********************************/

see(Handle):-
	check_handle(Handle, Handle1),
    set_input(Handle1),
    !.

see(File):-
    see(File, Handle),
    set_input(Handle).

see(File, Handle):-
   xcall('com.ugos.jiprolog.extensions.io.See2', [File, Handle]).

seeing(Handle):-
	check_handle(Handle, Handle1),
	current_input(Handle1),
	!.

seeing(File):-
    current_input(Handle),
    seeing(Handle, File).

seeing(Handle, File):-
    check_handle(Handle, Handle1),
    stream_property(Handle1, file_name(File)).

read(Term):-
    current_input(Handle),
    read(Handle, Term).

read(Handle, Term):-
	(	var(Handle) ->
		error(instantiation_error)
	;	check_handle(Handle, Handle1)
	),
	(	\+ current_stream(Handle1) ->
		error(existence_error(stream,Handle))
	;	\+ stream_property(Handle1, mode(read)) ->
		error(permission_error(input,stream,Handle))
	;	xcall('com.ugos.jiprolog.extensions.io.Read2', [Handle1, Term])
	).

read_term(Term, Options):-
	check_options(Options, Options),
    current_input(Handle),
    read(Handle, Term),
    options(Handle, Term, Options).

read_term(Handle, Term, Options):-
	check_options(Options, Options),
    read(Handle, Term),
    options(Handle, Term, Options).

read_clause(Term, Options):-
	check_options(Options, Options),
    current_input(Handle),
    read(Handle, Term),
    options(Handle, Term, Options).

read_clause(Handle, Term, Options):-
	check_options(Options, Options),
    read(Handle, Term),
    options(Handle, Term, Options).


get0(Char):-
    current_input(Handle),
    get0(Handle,Char).

get0(Handle, Char):-
    check_handle(Handle, Handle1),
    xcall('com.ugos.jiprolog.extensions.io.Get02', [Handle1, Char]).

get(C):-
    current_input(Handle),
    get(Handle, C).

get(Handle, C):-
    get0(Handle, C),
    C >= 32.

get_byte(B):-
    get0(B).

get_byte(Handle, B):-
    get0(Handle, B).

get_code(C):-
    get0(C),
    '$char'(C,_).

get_code(Handle, C):-
    get0(Handle, C),
    '$char'(C,_).

get_char(C):-
    current_input(Handle),
    get_char(Handle, C).

get_char(Handle, C):-
    get0(Handle, B),
    '$char'(B,C).


'$char'(-1, end_of_file) :-
	!.

'$char'(C, A) :-
	C > 31,
	!,
	char_atom(C, A).

'$char'(_, _) :-
	error(representation_error(character)).

peek_byte(B):-
    current_input(Handle),
    peek_byte(Handle, B).

peek_byte(Handle, B):-
    check_handle(Handle, Handle1),
    xcall('com.ugos.jiprolog.extensions.io.PeekByte2', [Handle1, B]).

peek_code(C):-
    peek_byte(C),
    '$char'(C,_).

peek_code(Handle, C):-
    peek_byte(Handle, C),
    '$char'(C,_).

peek_chars(C):-
    peek_byte(B),
    '$char'(B,C).

peek_chars(Handle, C):-
    peek_byte(Handle, B),
    '$char'(B,C).


skip(Char):-
    get0(C),
    (C = Char ; C = -1).

skip(Char):-
    skip(Char).

skip(Handle, Char):-
    get0(Handle, C),
    (C = Char ; C = -1),
    !.

skip(Handle, Char):-
    skip(Char).

seen:-
    current_input(Handle),
    seen(Handle),
    set_input(user_input).

seen(Handle):-
    check_handle(Handle, Handle1),
    xcall('com.ugos.jiprolog.extensions.io.Seen1', [Handle1]).

/**********************************
* Write Predicates
***********************************/

tell(Handle):-
    check_handle(Handle, Handle1),
    set_output(Handle1).

tell(File):-
    tell(File, Handle),
    set_output(Handle).

tell(File, Handle):-
   xcall('com.ugos.jiprolog.extensions.io.Tell2', [File, Handle]).

append(Handle):-
    check_handle(Handle, Handle1),
	set_output(Handle1),
	!.

append(File):-
    append(File, Handle),
    set_output(Handle).

append(File, Handle):-
   xcall('com.ugos.jiprolog.extensions.io.Append2', [File, Handle]).

telling(Handle):-
    check_handle(Handle, Handle1),
    current_output(Handle1),
    !.

telling(File):-
    current_output(Handle),
    telling(Handle, file).

telling(Handle, File):-
    check_handle(Handle, Handle1),
    stream_property(Handle1, file_name(File)).

write(Handle, Term):-
	(	var(Handle) ->
		error(instantiation_error)
	;	check_handle(Handle, Handle1)
	),
	(	\+ current_stream(Handle1) ->
		error(existence_error(stream,Handle))
	;	\+ stream_property(Handle1, mode(append)),
		\+ stream_property(Handle1, mode(write)) ->
		error(permission_error(output,stream,Handle))
	;	xcall('com.ugos.jiprolog.extensions.io.Write2', [Handle1, Term])
	).

write_canonical(Handle, Term):-
	(	var(Handle) ->
		error(instantiation_error)
	;	check_handle(Handle, Handle1)
	),
	(	\+ current_stream(Handle1) ->
		error(existence_error(stream,Handle))
	;	\+ stream_property(Handle1, mode(append)),
		\+ stream_property(Handle1, mode(write)) ->
		error(permission_error(output,stream,Handle))
	;	xcall('com.ugos.jiprolog.extensions.io.WriteCanonical2', [Handle1, Term])
	).


print(Term):-
    write(Term).

print(Handle, Term):-
    write(Handle, Term).

display(Term):-
    write_canonical(Term).

display(Handle, Term):-
    write_canonical(Handle, Term).

writeq(Term):-
    current_output(Handle),
    writeq(Handle, Term).

writeq(Handle, Term):-
    check_handle(Handle, Handle1),
    xcall('com.ugos.jiprolog.extensions.io.Writeq2', [Handle1, Term]).

writeln(Term):-
    write(Term), nl.

writeln(Handle, Term):-
    check_handle(Handle, Handle1),
    write(Handle1, Term),nl.

write_term(Term, Options):-
	check_options(Options, Options),
    current_output(Handle),
    write(Handle, Term),
    options(Handle, Term, Options).

write_term(Handle, Term, Options):-
	check_options(Options, Options),
    write(Handle, Term),
    options(Handle, Term, Options).

put(Handle, C):-
    number(C),
    !,
    name(Char, [C]),
    check_handle(Handle, Handle1),
    write(Handle, Char).

put(Handle, C):-
    atom(C),
    !,
    check_handle(Handle, Handle1),
    write(Handle, C).


put(C):-
    current_output(Handle),
    put(Handle, C).

put_byte(C):-
    put(C).

put_byte(Handle, C):-
    put(Handle, C).

put_char(C):-
    put(C).

put_char(Handle, C):-
    put(Handle, C).

put_code(C):-
    put(C).

put_code(Handle, C):-
    put(Handle, C).

tab(N):-
    current_output(Handle),
    tab(Handle, N).

tab(Handle, N):-
    N < 1,
    !.

tab(Handle, N):-
    N1 is N - 1,
    tab(N1),
    put(Handle, 32).

nl(Handle):-
    check_handle(Handle, Handle1),
    xcall('com.ugos.jiprolog.extensions.io.Nl1', [Handle1]).

flush_output(Handle):-
    check_handle(Handle, Handle1),
    xcall('com.ugos.jiprolog.extensions.io.FlushOutput1', [Handle1]).

flush_output:-
    current_output(Handle),
    flush_output(Handle).

told(Handle):-
    check_handle(Handle, Handle1),
    xcall('com.ugos.jiprolog.extensions.io.Told1', [Handle1]).

told:-
    current_output(Handle),
    told(Handle),
    set_output(user_output).


close(Handle, Options):-
	(	var(Handle)
	;	var(Options)
	),
	error(instantiation_error).

close(_, Options):-
	member(Option, Options),
	\+ ground(Option),
	error(instantiation_error).

close(_, Options):-
	\+ is_list(Options),
	error(type_error(list,Options)).

close(_, Options):-
	member(Option, Options),
	Option \== force(true),
	Option \== force(false),
	error(domain_error(close_option,Option)).

close(Handle, _):-
    check_handle(Handle, Handle1),
	\+ current_stream(Handle1),
	error(existence_error(stream,Handle)).

close(Handle, _Options):-
    close_(Handle).


close(Handle):-
	close(Handle, []).


close_(Handle):-
    check_handle(Handle, Handle1),
    stream_property(Handle1, input),
    !,
    seen(Handle1).

close_(Handle):-
    check_handle(Handle, Handle1),
    stream_property(Handle1, output),
    !,
    told(Handle1).


at_end_of_stream:-
    current_input(Handle),
    at_end_of_stream(Handle).

at_end_of_stream(Handle):-
	var(Handle),
	error(instantiation_error).

at_end_of_stream(Handle):-
	\+ atom(Handle),
	error(domain_error(stream_or_alias,Handle)).

at_end_of_stream(Handle):-
    check_handle(Handle, Handle1),
	(	\+ current_stream(Handle1) ->
		error(existence_error(stream,Handle))
	;	stream_property(Handle1, end_of_stream(E)) ->
		(	E = at
		;	E = past
		)
	;	peek_byte(Handle1, -1)
	).

current_output(Handle):-
	nonvar(Handle),
	(	\+ atom(Handle)
	;	\+ current_stream(Handle)
	),
	error(domain_error(stream,Handle)).

current_output(Handle):-
    xcall('com.ugos.jiprolog.extensions.io.CurrentOutput1', [Handle]).

current_input(Handle):-
	nonvar(Handle),
	(	\+ atom(Handle)
	;	\+ current_stream(Handle)
	),
	error(domain_error(stream,Handle)).

current_input(Handle):-
    xcall('com.ugos.jiprolog.extensions.io.CurrentInput1', [Handle]).

current_stream(Handle):-
	 xcall('com.ugos.jiprolog.extensions.io.CurrentStream1', [Handle]).

set_output(Handle):-
	var(Handle),
	error(instantiation_error).

set_output(Handle):-
	\+ atom(Handle),
	error(domain_error(stream_or_alias,Handle)).

set_output(Handle):-
	check_handle(Handle, Handle1),
	(	\+ current_stream(Handle1) ->
		error(existence_error(stream,Handle))
	;	\+ stream_property(Handle1, mode(append)),
		\+ stream_property(Handle1, mode(write)) ->
		error(permission_error(output,stream,Handle))
	).

set_output(Handle):-
    xcall('com.ugos.jiprolog.extensions.io.SetOutput1', [Handle]).

set_input(Handle):-
	var(Handle),
	error(instantiation_error).

set_input(Handle):-
	\+ atom(Handle),
	error(domain_error(stream_or_alias,Handle)).

set_input(Handle):-
	check_handle(Handle, Handle1),
	(	\+ current_stream(Handle1) ->
		error(existence_error(stream,Handle))
	;	\+ stream_property(Handle1, mode(read)) ->
		error(permission_error(input,stream,Handle))
	).

set_input(Handle):-
    xcall('com.ugos.jiprolog.extensions.io.SetInput1', [Handle]).

/*******************/

access_file(File, none):-!.

access_file(File, Mode):-
   xcall('com.ugos.jiprolog.extensions.io.AccessFile2', [File, Mode]).

exists_file(File):-
    access_file(File, exist).

exists_directory(File):-
    access_file(File, directory).

same_file(File1, File2):-
    absolute_file_name(File1, File),
    absolute_file_name(File2, File).

working_directory(X, Y):-
    chdir(X),
    chdir(Y).

searchpath(X):-
    chdir(X).

absolute_file_name(File, Abs):-
    xcall('com.ugos.jiprolog.extensions.io.AbsoluteFileName2', [File, Abs]).

absolute_file_name(File, Abs, Opts):-
    xcall('com.ugos.jiprolog.extensions.io.AbsoluteFileName2', [File, Abs]).

is_absolute_file_name(File):-
    absolute_file_name(File, File).

file_attributes(File, Name, Ext, Dir, Abs, Size, Time):-
    xcall('com.ugos.jiprolog.extensions.io.FileAttributes7', [File, Name, Ext, Dir, Abs, Size, Time]).

file_name_extension(File, Ext):-
    file_attributes(File, _, Ext, _, _, _, _).

size_file(File, Size):-
    exists_file(File),
    !,
    file_attributes(File, _, _, _, _, Size, _).

size_file(File, _Size):-
    error(existence_error(stream, File)).

time_file(File, Time):-
    exists_file(File),
    !,
    file_attributes(File, _, _, _, _, _, Time).

time_file(_File, _Size):-
    error(system_error(not_implemented)).

file_directory_name(File, Directory):-
    file_attributes(File, _, _, Directory, _, _, _).

file_base_name(File, Base):-
    file_attributes(File, Base, _, _, _, _, _).

delete_file(File):-
    xcall('com.ugos.jiprolog.extensions.io.DeleteFile1', [File]).

delete_directory(Dir):-
    xcall('com.ugos.jiprolog.extensions.io.DeleteFile1', [Dir]).

rename_file(File, NewFile):-
    xcall('com.ugos.jiprolog.extensions.io.RenameFile2', [File, NewFile]).

dir:-
    chdir(CurDir),
    write('File list in '),
    write(CurDir), nl,
    dir(X),
    write_dir(X).

write_dir([]):-!.

write_dir([X|Xs]):-
    write(X), nl,
    write_dir(Xs).

dir(X):-
    xcall('com.ugos.jiprolog.extensions.io.Dir1', [X]).

cd(Dir):-
    chdir(Dir).

make_directory(Dir):-
    xcall('com.ugos.jiprolog.extensions.io.MakeDirectory1', [Dir]).



% stream properties


stream_property(Handle, position(line(Line))):-
	current_stream(Handle),
	xcall('com.ugos.jiprolog.extensions.io.StreamPosition3', [Handle, _P, Line]).

%stream_property(Handle, end_of_stream(X)):-
%	current_stream(Handle),
%	xcall('com.ugos.jiprolog.extensions.io.EOF2', [Handle, X]).

stream_property(Handle, Prop):-
	current_stream(Handle),
	xcall('com.ugos.jiprolog.extensions.io.StreamProperty3', [get, Handle, Prop]).

stream_property(Handle, _):-
	\+ nonvar(Handle),
	\+ current_stream(Handle),
	error(domain_error(stream,Handle)).

set_stream_property(Handle, Prop):-
    xcall('com.ugos.jiprolog.extensions.io.StreamProperty3', [set, Handle, Prop]).

set_stream_properties(Handle, []):-!.
set_stream_properties(Handle, [Prop|Rest]):-
	set_stream_property(Handle, Prop),
    set_stream_properties(Handle, Rest).

% check for aliases
check_handle(Alias, Handle):-
   nonvar(Alias),
   stream_property(Handle, alias(Alias)),
   !.

check_handle(Alias, Alias):-
   nonvar(Alias),
   !.

check_handle(_, _):-
   error(instantiation_error,_).



% not supported

seek(_,_,_,_):-
	error(system_error(not_supported)).

set_stream_position(Handle, _):-
	var(Handle),
	error(instantiation_error).

set_stream_position(_, Position):-
	var(Position),
	error(instantiation_error).

set_stream_position(Handle, _):-
	check_handle(Handle, Handle1),
	current_stream(Handle1),
	error(permission_error(reposition, stream, Handle)).

set_stream_position(Handle, _):-
	error(existence_error(stream,Handle)).

set_stream(Handle, Prop):-
	check_handle(Handle, Handle1),
	set_stream_property(Handle1, Prop).

char_atom(B, C):-
    xcall('com.ugos.jiprolog.extensions.io.CharAtom2', [B, C]).


check_options(Options, _):-
	var(Options),
    error(instantiation_error).

check_options([], _) :-
	!.

check_options([Option| Options], OriginalOptions):-
	!,
	(	var(Option) ->
		error(instantiation_error)
	;	check_options(Options, OriginalOptions)
	).

check_options(_, OriginalOptions):-
	error(type_error(list,OriginalOptions)).


options(_Handle, _Term, V):-
	var(V),
    error(instantiation_error).

options(_Handle, _Term, []):-!.


options(Handle, Term, [Opt|Options]):-
    check_handle(Handle, Handle1),
    option(Handle1, Term, Opt),
    !,
    options(Handle, Term, Options).

options(Handle, Term, [_Opt|Options]):-
    options(Handle, Term, Options),
    !.

%option(Handle, Term, quoted(false)).
%option(Handle, Term, backquoted_string(false)).
%option(Handle, Term, character_escapes(false)).
%option(Handle, Term, ignore_ops(true)).
option(Handle, Module:Term, module(Module)).
option(Handle, Term, module(user)).
%option(Handle, Term, numbervars(false)).
%option(Handle, Term, portray(false)).
%option(Handle, Term, max_depth(0)).
option(Handle, Term, variables(X)):-
    free_variables(Term, X),
    !.
option(Handle, Term, variables([])).
%option(Handle, Term, variable_names([])).
%option(Handle, Term, singletons([])).
option(Handle, Term, syntax_errors('error')).
%option(Handle, Term, double_quotes(false)).
%option(Handle, Term, term_position(0)).
%option(Handle, Term, subterm_positions(_)).


% user stream properties
%:- jipxio:set_stream_properties(user_output, [mode(write), output, alias(user_output), file_name(user_output), eof_action(eof_code), type(text), reposition(false)]).
%:- jipxio:set_stream_properties(user_error, [mode(write), output, alias(user_error), file_name(user_error), eof_action(eof_code), type(text), reposition(false)]).
%:- jipxio:set_stream_properties(user_input, [mode(read), input, alias(user_input), file_name(user_input), eof_action(eof_code), type(text), reposition(false)]).



