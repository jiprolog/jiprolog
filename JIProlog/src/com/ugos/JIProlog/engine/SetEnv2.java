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

final class SetEnv2 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        PrologObject termKey = getParam(1);
        PrologObject termValue = getParam(2);

        if(termKey instanceof Variable)
        {
        	termKey = ((Variable)termKey).getObject();
        }

        if(termKey == null)
        	throw new JIPParameterUnboundedException(1);

        if(termValue instanceof Variable)
        {
        	termValue = ((Variable)termValue).getObject();
        }

        if(termValue == null)
        	throw new JIPParameterUnboundedException(2);

        String key = null;
        if(termKey instanceof Atom || termKey instanceof PString)
        	key = termKey.toString(getJIPEngine());
        else
        	throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM_OR_STRING);

        Object val;
        if(termValue instanceof Atom || termValue instanceof PString)
        	val = termValue.toString(getJIPEngine());
        else if(termValue instanceof Expression)
        	val = ((Expression)termValue).getValue();
        else
        	val = termValue;

        getJIPEngine().setEnvVariable(key, val);

        return true;
    }
}
