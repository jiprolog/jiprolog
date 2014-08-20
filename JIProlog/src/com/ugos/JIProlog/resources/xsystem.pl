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
:-module(jipxsystem, [sleep/1, shell/2, shell/1, statistics/0, get_time/1, time/1, time/4, time/5, time/7, time/8,
                      convert_time/8, date/3, date/4, ms/2, wait/1]).
                     
:-'$custom_built_in'([sleep/1, shell/2, shell/1, statistics/0, get_time/1, time/1, time/4, time/5, time/7, time/8,
                      convert_time/8, date/3, date/4, ms/2, wait/1]).
                      
:-assert(ver(jipxsystem, '3.0.1')).

sleep(Millis):-
    xcall('com.ugos.JIProlog.extensions.system.Sleep1', [Millis]).

wait(Millis):-
    xcall('com.ugos.JIProlog.extensions.system.Sleep1', [Millis]).

shell(Command, Status):-
    xcall('com.ugos.JIProlog.extensions.system.Shell2', [Command, Status]).

shell(Command):-
    shell(Command, 0).
    
statistics:-
    xcall('com.ugos.JIProlog.extensions.system.Statistics0', []).
        
time(CurTime):-
    CurTime is cputime.

get_time(CurTime):-
    time(CurTime).

time(Time, Year, Month, Day, Hour, Min, Second, Millis):-
    xcall('com.ugos.JIProlog.extensions.system.Time8', [Time, Year, Month, Day, Hour, Min, Second, Millis]).

time(Year, Month, Day, Hour, Min, Second, Millis):-
    time(Time),
    xcall('com.ugos.JIProlog.extensions.system.Time8', [Time, Year, Month, Day, Hour, Min, Second, Millis]).

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


%*************************************

