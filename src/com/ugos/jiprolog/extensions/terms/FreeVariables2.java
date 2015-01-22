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

package com.ugos.jiprolog.extensions.terms;

import com.ugos.jiprolog.engine.*;

import java.util.*;

public class FreeVariables2 extends JIPXCall
{
    public final boolean unify(final JIPCons input, Hashtable varsTbl)
    {
        JIPTerm term = input.getNth(1);

        JIPVariable vars[];
        // check if input is a variable
        if (term instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)term).isBounded())
            {
            	vars = new JIPVariable[1];
            	vars[0] = (JIPVariable)term;
            }
            else
            {
                //extracts the term
                term = ((JIPVariable)term).getValue();
                vars = term.getVariables();
            }
        }
        else
        {
        	vars = term.getVariables();
        }

        Hashtable<String, JIPVariable> vartbl = new Hashtable<String, JIPVariable>();
        JIPList varList = null;
        for(int i = 0; i < vars.length; i++)
        {
        	JIPVariable var = vars[i];//.getLastVariable();
        	if(!vartbl.containsKey(var.toString()))
        	{
        		varList = JIPList.create(var, varList);
        		vartbl.put(var.toString(), var);
        	}
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
}

