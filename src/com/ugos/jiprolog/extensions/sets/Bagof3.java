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

public class Bagof3 extends Findall3
{
    private Vector  m_solVect = null;
    private JIPCons m_freeVars;
    private JIPList m_solList = null;

    public final boolean unify(final JIPCons input, Hashtable varsTbl)
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
                throw new JIPInstantiationException(1);
            }
            else
            {
                //extracts the term
                query = ((JIPVariable)query).getValue();
            }
        }

        // check if input is a variable
        if (term instanceof JIPVariable)
        {
            // try to extract the term
            if(((JIPVariable)term).isBounded())
            {
                //extracts the term
                term = ((JIPVariable)term).getValue();
            }
        }

        JIPTerm res = input.getNth(3);
                

        if(m_solVect == null)
        {
            m_solVect = collect(query);
            m_freeVars = extractFreeVars(query);
        }

        if(m_solVect.isEmpty())
            return false;

        m_solList = null;

        JIPTerm sol;
        m_freeVars.clear();
        int j = 0;
        while(j < m_solVect.size())
        {
            sol = (JIPTerm)m_solVect.elementAt(j);
            if(query.unify(sol, varsTbl))
            {
                m_solList = JIPList.create((JIPTerm)term.clone(), m_solList);
                // clear all free variables
                term.clear();
                m_freeVars.clear();
                // remove the element
                m_solVect.removeElementAt(j);
            }
            else
            {
                j++;
            }
        }

        if(m_solList == null)
            return false;
            //m_solList = JIPList.NIL;
        else
            m_solList = m_solList.reverse();

//        res.clear();

        return res.unify(m_solList, varsTbl);
    }

    public final boolean hasMoreChoicePoints()
    {
        return m_solList != JIPList.NIL && (super.hasMoreChoicePoints() || !m_solVect.isEmpty());
    }

    private final JIPCons extractFreeVars(JIPTerm obj)
    {
        if(obj instanceof JIPFunctor)
        {
            if (((JIPFunctor)obj).getName().equals("^"))
                return JIPCons.create(((JIPFunctor)obj).getParams().getHead(), extractFreeVars(((JIPFunctor)obj).getParams().getTail()));
            else
                return JIPCons.NIL;
        }
        else if(obj instanceof JIPCons)
        {
            JIPCons head = extractFreeVars(((JIPCons)obj).getHead());
            if(head != null)
                return JIPCons.append(head, extractFreeVars(((JIPCons)obj).getTail()));
            else
                return extractFreeVars(((JIPCons)obj).getTail());
        }
        else
            return JIPCons.NIL;
    }
}

