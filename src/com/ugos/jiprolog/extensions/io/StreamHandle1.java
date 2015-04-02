/*
 * Copyright (C) 1999-2004 By Ugo Chirico
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


package com.ugos.jiprolog.extensions.io;

import com.ugos.jiprolog.engine.*;

import java.io.*;
import java.util.*;

public final class StreamHandle1 extends JIPXCall
{
	private static Hashtable<String, Vector<JIPTerm>> streamPropertyTbl = new Hashtable<String, Vector<JIPTerm>>();

	private Vector<JIPTerm> propsVect;
	private int index;

    public final boolean unify(final JIPCons params, Hashtable<JIPVariable, JIPVariable> varsTbl)
    {
        // get first parameter
        JIPTerm handle = params.getNth(1).getValue();
        if(handle == null)
            throw new JIPParameterUnboundedException(1);

        if(!(handle instanceof JIPAtom))
            throw new JIPTypeException(JIPTypeException.ATOM, handle);

        JIPTerm prop = params.getNth(2);
        JIPTerm prop1 = prop.getValue();

        if(propsVect == null)
        {
        	propsVect = streamPropertyTbl.get(((JIPAtom)handle).getName());
        	if(propsVect == null)
        		return false;

        	index = 0;
        }

        while(index < propsVect.size())
        {
        	JIPTerm term = propsVect.elementAt(index);
        	index++;

        	if(prop.unifiable(term))
        		return prop.unify(term, varsTbl);
        }

        return false;
    }

    public boolean hasMoreChoicePoints()
    {
        return index < propsVect.size();
    }
}

