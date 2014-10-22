/******************************************************************
 * 18/06/2004
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
 * Such file should be used together with the package JIPxerr.zip
 * Defined ISO predicates:
 *  catch/3
 *  throw/1
 * Author: Ugo Chirico 18/06/04
*******************************************************************/

:-module(jipxexception, [catch/3, throw/1]).

:-'$custom_built_in'([catch/3, throw/1]).

:-assert(ver(jipxexception, '3.0.2')).

catch(Goal, Catcher, RecoverGoal):-
   xcall('com.ugos.jiprolog.extensions.exception.JIPCatch3', [Goal, Catcher, RecoverGoal]).

throw(Exception):-
   xcall('com.ugos.jiprolog.extensions.exception.JIPThrow1', [Exception]).


