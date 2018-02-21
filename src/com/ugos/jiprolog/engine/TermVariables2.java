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

package com.ugos.jiprolog.engine;

import com.ugos.jiprolog.engine.*;

import java.util.*;

public class TermVariables2 extends BuiltIn
{
	 Hashtable<Long, Variable> vartbl = new Hashtable<Long, Variable>();

    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
    	 PrologObject term = getParam(1);
    	 PrologObject list = getParam(2);
    
        if(list instanceof Variable)
        {
        	list = ((Variable)list).getObject();
        }

    	if(list != null)
    	{
        	if(!(list instanceof List))
        		throw new JIPTypeException(JIPTypeException.LIST, list);
        	if(!(((List)list).isClosedOrPartial()))
        		throw new JIPTypeException(JIPTypeException.LIST, list);
    	}

        Vector<Variable> varsVect = new Vector<Variable>();

        addVariables(term, varsVect);

        List varList = null;
        for(int i = 0; i < varsVect.size(); i++)
        {
        	Variable var = varsVect.elementAt(i);//.getLastVariable();
    		varList = new List(var, varList);
        }

        if(varList == null)
        {
            varList = List.NIL;
        }
        else
        {
            varList = (List)varList.reverse();
        }

        return getParam(2).unify(varList, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }

    private void addVariables(PrologObject term, Vector<Variable> varsVect)
    {
    	Variable vars[] = term.getVariables();

        for(int i = 0; i < vars.length; i++)
        {
        	Variable var = vars[i];
        	if(var.isBounded())
        	{
        		addVariables(var.getObject(), varsVect);
        	}
        	else
        	{
        		if(!vartbl.containsKey(new Long(var.getAddress())) && !vartbl.containsKey(new Long(var.lastVariable().getAddress())))
            	{
        		
        			varsVect.addElement(var);
        			
        			vartbl.put(new Long(var.getAddress()), var);
            		vartbl.put(new Long(var.lastVariable().getAddress()), var);
            		
            	}
        	}
        }
    }
}

