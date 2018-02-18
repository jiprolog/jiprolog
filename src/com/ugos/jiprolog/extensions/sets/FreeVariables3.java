/*****************************************
 * 27/03/2003
 *
 * Copyright (C) 1999-2003 Ugo Chirico
 * http://www.ugochirico.com
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
 *****************************************/

package com.ugos.jiprolog.extensions.sets;

import com.ugos.jiprolog.engine.*;

import java.util.*;

public class FreeVariables3 extends JIPXCall
{
	 Hashtable<Long, JIPVariable> vartbl = new Hashtable<Long, JIPVariable>();

    public final boolean unify(final JIPCons input, Hashtable varsTbl)
    {
        JIPTerm generator = input.getNth(1);
        JIPTerm template = input.getNth(2);
        JIPTerm vars = input.getNth(3);

        if(generator instanceof JIPVariable)
        {
        	generator = ((JIPVariable)generator).getValue();
        }

        
        if(generator == null)
        {
        	throw new JIPInstantiationException(1);
        }

        Vector<JIPVariable> varsVect = new Vector<JIPVariable>();

        addVariables(term, varsVect);

        JIPList varList = null;
        for(int i = 0; i < varsVect.size(); i++)
        {
        	JIPVariable var = varsVect.elementAt(i);//.getLastVariable();
    		varList = JIPList.create(var, varList);
        }

        if(varList == null)
        {
            varList = JIPList.NIL;
        }
        else
        {
            varList = varList.reverse();
        }

        return input.getNth(2).unify(varList, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }

    private void addVariables(JIPTerm term, Vector<JIPVariable> varsVect)
    {
    	JIPVariable vars[] = term.getVariables();

        for(int i = 0; i < vars.length; i++)
        {
        	JIPVariable var = vars[i];//.getLastVariable();
        	if(var.isBounded())
        	{
        		addVariables(var.getValue(), varsVect);
        	}
        	else
        	{
        		if(!vartbl.containsKey(new Long(var.getAddress())))
            	{
        			varsVect.addElement(var);
            		vartbl.put(new Long(var.getAddress()), var);
            	}
        	}
        }
    }
}

