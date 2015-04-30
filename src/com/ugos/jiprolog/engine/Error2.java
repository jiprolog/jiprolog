/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico - http://www.ugochirico.com
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the Affero GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Affero GNU General Public License for more details.
 *
 * You should have received a copy of the Affero GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package com.ugos.jiprolog.engine;

import java.util.Hashtable;

final class Error2 extends BuiltIn
{
    public final boolean unify(final Hashtable varsTbl)
    {
    	PrologObject errorTerm = getRealTerm(getParam(1));
    	PrologObject caller = getRealTerm(getParam(2));

    	if(errorTerm == null)
    		throw new JIPInstantiationException(1);

    	if(caller == null)
    		throw new JIPInstantiationException(2);

    	JIPRuntimeException exception = new JIPRuntimeException(errorTerm);
    	exception.setTerm(caller);

//    	if(error.startsWith("instantiation_error"))
//    	{
//    		throw new JIPParameterUnboundedException();
//    	}
//    	else if(error.startsWith("uninstantiation_error"))
//    	{
//    		throw new JIPParameterUnboundedException();
//    	}
//    	else if(error.startsWith("type_error"))
//    	{
//    		throw new JIPTypeException(typeError, culprit);
//    	}
//    	else if(error.startsWith("domain_error"))
//    	{
//
//    	}
//    	else if(error.startsWith("permission_error"))
//    	{
//
//    	}
//    	else if(error.startsWith("representation_error"))
//    	{
//
//    	}
//    	else if(error.startsWith("evaluation_error"))
//    	{
//
//    	}
//    	else if(error.startsWith("syntax_error"))
//    	{
//
//    	}
//    	else
//    	{
//
//    	}
//
//    	exception.setTerm(caller);

        throw exception;
    }
}
