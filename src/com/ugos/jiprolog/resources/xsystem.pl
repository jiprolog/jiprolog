/******************************************************************
 * 27/03/2002
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

/******************************************************************
 * utility extension package v1.1.0
 ******************************************************************/
:-module(jipxsystem, [sleep/1, shell/2, shell/1, getenv/2, statistics/0, get_time/1, time/1, time/4, time/5, time/7, time/8,
                      convert_time/8, date/3, date/4, ms/2, wait/1]).

:-'$custom_built_in'([sleep/1, shell/2, shell/1, getenv/2, statistics/0, get_time/1, time/1, time/4, time/5, time/7, time/8,
                      convert_time/8, date/3, date/4, ms/2, wait/1]).

:-assert(ver(jipxsystem, '3.0.2')).

sleep(Millis):-
    xcall('com.ugos.jiprolog.extensions.system.Sleep1', [Millis]).

wait(Millis):-
    xcall('com.ugos.jiprolog.extensions.system.Sleep1', [Millis]).

shell(Command, Status):-
%	split_command(Command, List),
    xcall('com.ugos.jiprolog.extensions.system.Shell2', [Command, Status]).

shell(Command):-
%	split_command(Command, List),
    shell(Command, 0).

getenv(Variable, Value) :-
	(	invoke('java.lang.System', getenv('java.lang.String'), [Variable], Value),
		Value \== [] ->
		true
	;	% check if the environment variable value is passed as a property
		invoke('java.lang.System', getProperty('java.lang.String'), [Variable], Value),
		Value \== []
	).

statistics:-
    xcall('com.ugos.jiprolog.extensions.system.Statistics0', []).

time(CurTime):-
    CurTime is cputime.

get_time(CurTime):-
    time(CurTime).

time(Time, Year, Month, Day, Hour, Min, Second, Millis):-
    xcall('com.ugos.jiprolog.extensions.system.Time8', [Time, Year, Month, Day, Hour, Min, Second, Millis]).

time(Year, Month, Day, Hour, Min, Second, Millis):-
    time(Time),
    xcall('com.ugos.jiprolog.extensions.system.Time8', [Time, Year, Month, Day, Hour, Min, Second, Millis]).

convert_time(Time, Year, Month, Day, Hour, Min, Second, Millis):-
    time(Time, Year, Month, Day, Hour, Min, Second, Millis).

ms(Call, Ms):-
    time(Ms1),
    call(Call),
    time(Ms2),
    Ms is Ms2 - Ms1.

date(Year, Month, Day):-
    time(Year, Month, Day, _,_,_,_).

date(Time, Year, Month, Day):-
    time(Time, Year, Month, Day, _,_,_,_).

time(Time, Hour, Minute, Second, Millisecond ):-
    time(Time, _,_,_,Hour, Minute, Second, Millisecond).

time(Hour, Minute, Second, Millisecond ):-
    time(_,_,_,Hour, Minute, Second, Millisecond).

split_command(Command, List) :-
	atom_chars(Command, Chars),
	split_command_(Chars, Lists),
	lists_to_atoms(Lists, List).

split_command_([], [[]]).
split_command_([' '| Chars], [[]| List]) :-
	!,
	split_command_(Chars, List).
split_command_([Char| Chars], [[Char|Tail]| List]) :-
	split_command_(Chars, [Tail| List]).

lists_to_atoms([], []).
lists_to_atoms([List| Lists], [Atom| Atoms]) :-
	atom_chars(Atom, List),
	lists_to_atoms(Lists, Atoms).

%*************************************

