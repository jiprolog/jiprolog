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

import java.util.Hashtable;

final class AcyclicTerm1 extends BuiltIn
{
	private Hashtable<PrologObject, PrologObject> termTbl = new Hashtable<PrologObject, PrologObject>();

    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        final PrologObject term = getRealTerm(getParam(1));

        return acyclic(term);
    }

    private boolean acyclic(PrologObject term)
    {
    	if(term == null)
    	{
    		return false;
    	}
    	else if(termTbl.containsKey(term))
    	{
    		return true;
    	}
    	else if(term instanceof ConsCell)
        {
    		return acyclic(((ConsCell)term).getHead()) || acyclic(((ConsCell)term).getTail());
    	}
        else if(term instanceof Variable)
        {
        	termTbl.put(term, term);
        	return acyclic(((Variable)term).getObject());
        }

    	return false;
    }
}
