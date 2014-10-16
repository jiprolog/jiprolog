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

package com.ugos.JIProlog.engine;
import java.util.*;

final class Env2 extends BuiltIn
{
	PrologObject termKey;
	Enumeration<String> en;
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        termKey = getParam(1);
        final PrologObject termValue = getParam(2);

        PrologObject termKey1 = termKey;
        if(termKey instanceof Variable)
        {
        	termKey1 = ((Variable)termKey).getObject();
        }

        if(termKey1 != null)
        {
	        Object val = getJIPEngine().getEnvVariable(termKey1.toString(getJIPEngine()));

	        if(val == null)
	        	return false;
	        else if(val instanceof String)
	        	return termValue.unify(Atom.createAtom((String)val), varsTbl);
	        else if(val instanceof Number)
	        	return termValue.unify(Expression.createNumber(((Number)val).doubleValue()), varsTbl);
	        else
	        	return termValue.unify(Atom.createAtom("#" + val.hashCode()), varsTbl);
        }
        else
        {
        	if(en == null)
        		en = getJIPEngine().getEnvVariableNames();

        	if(en.hasMoreElements())
        	{
        		String key = en.nextElement();
        		Object val = getJIPEngine().getEnvVariable(key);

        		if(val instanceof String)
        		{
        			return termKey.unify(Atom.createAtom(key), varsTbl) && termValue.unify((Atom.createAtom((String)val)), varsTbl);
        		}
        		else if(val instanceof Number)
        		{
        			return termKey.unify(Atom.createAtom(key), varsTbl) && termValue.unify(Expression.createNumber(((Number)val).doubleValue()), varsTbl);
        		}
        		else
        		{
        			return termKey.unify(Atom.createAtom(key), varsTbl) && termValue.unify((Atom.createAtom("#" + val.hashCode())), varsTbl);
        		}
        	}
        	else
        	{
        		return false;
        	}
        }
    }

	@Override
	public boolean hasMoreChoicePoints()
	{
		return (en != null) && en.hasMoreElements();
	}
}
