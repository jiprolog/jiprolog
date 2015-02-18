/*****************************************
 *
 * Copyright (C) 1999-2004 By Ugo Chirico
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

public class Findall3 extends JIPXCall
{
    private JIPQuery m_query;

    public boolean unify(final JIPCons input, Hashtable varsTbl)
    {
    	JIPCons input1 = (JIPCons)input.clone();

        JIPTerm term = input1.getNth(1);
        JIPTerm query = input1.getNth(2);

        // check if input is a variable
        if (query instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)query).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                query = ((JIPVariable)query).getValue();
            }
        }

        final JIPTerm res = input.getNth(3);

        final Vector solVect = collect(query);
        JIPTerm sol;
        Enumeration en;
        JIPList solList = null;
        for(int i = 0; i < solVect.size(); i++)
        {
            sol = (JIPTerm)solVect.elementAt(i);
            if(query.unify(sol, varsTbl))
            {
                solList = JIPList.create((JIPTerm)term.clone(), solList);
                en = varsTbl.elements();
                while(en.hasMoreElements())
                    ((JIPTerm)en.nextElement()).clear();
            }
            else
            {
                throw new JIPRuntimeException("Unexpected error in Findall/3");
            }
        }

        if(solList == null)
            solList = JIPList.NIL;
        else
            solList = solList.reverse();

        return res.unify(solList, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return m_query == null;  // never started
    }

    protected final Vector collect(JIPTerm query)
    {
        //System.out.println("query " + query);

        final Vector solVect = new Vector();
        m_query = getJIPEngine().openSynchronousQuery(query);
        JIPTerm sol;
        while((sol = m_query.nextSolution()) != null)
        {
            solVect.addElement(sol);
        }

        m_query.close();

        //System.out.println("query close" + query);

        return solVect;
    }
}

