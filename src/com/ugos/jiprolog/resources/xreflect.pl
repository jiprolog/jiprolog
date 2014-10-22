/*************************
 * Reflect library
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

/******************************************************************
 * Reflect extension package v3.0.0
 ******************************************************************/

:-assert(ver(jipxreflect, '3.0.0')).

:-module(jipxreflect,
         [create_object/3, release_object/1, invoke/4, get/3, set/3, get_class/2,
          get_constructors/2, get_methods/2, get_fields/2]).

create_object(ClassName, Params, Handle):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPCreateObject3', [ClassName, Params, Handle]).

release_object(Handle):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPReleaseObject1', [Handle]).

invoke(Handle, MethodName, Params, RetVal):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPInvoke4', [Handle, MethodName, Params, RetVal]).

get(Handle, FieldName, Val):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPGet3', [Handle, FieldName, Val]).

set(Handle, FieldName, Val):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPSet3', [Handle, FieldName, Val]).

get_class(Handle, Class):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPGetClass2', [Handle, Class]).

get_constructors(Handle, Constructors):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPGetConstructors2', [Handle, Constructors]).

get_methods(Handle, Methods):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPGetMethods2', [Handle, Methods]).

get_fields(Handle, Fields):-
    xcall('com.ugos.jiprolog.extensions.reflect.JIPGetFields2', [Handle, Fields]).

