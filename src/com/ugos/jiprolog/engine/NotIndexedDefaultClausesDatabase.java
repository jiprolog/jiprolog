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

class NotIndexedDefaultClausesDatabase extends DefaultClausesDatabase
{
    private String m_strFullName;
    private GlobalDB m_gdb;
    public NotIndexedDefaultClausesDatabase(final String strFunctName, final int nArity, final String strFullName, GlobalDB gdb)
    {
    	super(strFunctName, nArity);
        m_strFullName = strFullName;
        m_gdb = gdb;
    }

    public void setAttributes(final String strAttribs)
    {
        // do nothing
    }

    public synchronized boolean addClauseAtFirst(final JIPClause clause)
    {
        m_clausesVector.add(0, (Clause)clause.getTerm());
        if(getArity() > 0 && m_clausesVector.size() > 2)
        	m_gdb.makeIndexed(this);

        return true;
    }

    public synchronized boolean addClause(final JIPClause clause)
    {
        m_clausesVector.add((Clause)clause.getTerm());
        if(getArity() > 0 && m_clausesVector.size() > 2)
        	m_gdb.makeIndexed(this);

        return true;
    }

	public String getFullName() {
		return m_strFullName;
	}
}