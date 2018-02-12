/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico
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

import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.Hashtable;

final class AcyclicTerm1 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        return acyclic(getParam(1));
    }

    public static boolean acyclic(PrologObject term)
    {
    	Hashtable<PrologObject, PrologObject> termTbl = new Hashtable<PrologObject, PrologObject>();
    	
    	return acyclic(term, termTbl);
    }

    private static boolean acyclic(PrologObject term, Hashtable<PrologObject, PrologObject> termTbl)
    {
	
    	if(term == null)
    	{
    		return true;
    	}
    	else if(termTbl.containsKey(term))
    	{
    		return false;
    	}
		else if(term instanceof Functor)
		{
			return acyclic(((Functor)term).getParams(), termTbl);
		}
		else if(term instanceof List)
		{
    		return acyclic(((List)term).getHead(), termTbl) && acyclic(((List)term).getTail(), termTbl);
		}
    	else if(term instanceof ConsCell)
        {
    		return acyclic(((ConsCell)term).getHead(), termTbl) && acyclic(((ConsCell)term).getTail(), termTbl);
    	}
        else if(term instanceof Variable)
        {
        	termTbl.put(term, term);
        	return acyclic(((Variable)term).getObject(), termTbl);
        }

    	return true;
    }
}
