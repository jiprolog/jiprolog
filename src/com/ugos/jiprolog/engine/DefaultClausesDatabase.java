/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico - http://www.ugochirico.com
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

import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.Vector;

final class DefaultClausesDatabase extends JIPClausesDatabase
{
    private final Vector<Clause> m_clausesVector;

    public DefaultClausesDatabase(final String strFunctName, final int nArity)
    {
        setFunctor(strFunctName, nArity);

        m_clausesVector = new Vector<Clause>();
    }

    public final void setAttributes(final String strAttribs)
    {
        // do nothing
    }

    public final synchronized boolean addClauseAt(final int nPos, final JIPClause clause)
    {
        m_clausesVector.insertElementAt((Clause)clause.getTerm(), nPos);
        return true;
    }

    public final synchronized boolean addClause(final JIPClause clause)
    {
        m_clausesVector.addElement((Clause)clause.getTerm());
        return true;
    }

    public final synchronized boolean removeClause(final JIPClause clause)
    {
        m_clausesVector.removeElement(clause.getTerm());
        return true;
    }

    public final synchronized Enumeration clauses()
    {
    	if(getJIPEngine().isImmediateUpdateSemantics())
    		return m_clausesVector.elements();
    	else
    		return clausesLSU();
    }

    private final synchronized Enumeration clausesLSU()
    {
    	return ((Vector<Clause>)m_clausesVector.clone()).elements();

//    	return new Enumeration<Clause>()
//    	{
//            int count = 0;
//
//            public boolean hasMoreElements() {
//                return count < clausesVector.size();
//            }
//
//            public Clause nextElement() {
//                synchronized (clausesVector) {
//                    if (count < clausesVector.size()) {
//                        return clausesVector.elementAt(count++);
//                    }
//                }
//                throw new NoSuchElementException("Vector Enumeration");
//            }
//        };
    }
}
