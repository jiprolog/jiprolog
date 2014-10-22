/***************************************************************
 * 03/12/2002
 *
 * Copyright (C) 1999-2003 Ugo Chirico
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
 ****************************************************************/

:-assert(ver(jipxdb, '3.0.0')).

/**********************
 * JDBC table
 ***********************/

declare_extern(jdbc, Predicate, Params):-
    !,
    extern(Predicate, 'com.ugos.jiprolog.extensions.database.JDBCClausesDatabase', Params).

/**********************
 * Text table
 ***********************/

declare_extern(text, Predicate, Params):-
    !,
    extern(Predicate, 'com.ugos.jiprolog.extensions.database.TextClausesDatabase', Params).

/**********************
 * Prolog table
 ***********************/

declare_extern(prolog, Predicate, Params):-
    !,
    extern(Predicate, 'com.ugos.jiprolog.extensions.database.PrologClausesDatabase', Params).


