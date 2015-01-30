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

package com.ugos.jiprolog.engine;

/** Exception raised when a built-in predicate find an unexpected unbonded variable.
 * @version 3.0
 * @author Ugo Chirico 2005<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPRuntimeException
 */
public class JIPParameterUnboundedException extends JIPRuntimeException
{
    private int m_nParam = -1;

    /** Constructor
     * Constructs a new JIPParameterUnboundedException
     * @param nParam the index of the bad parameter
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public JIPParameterUnboundedException(final int nParam)
    {
        super(create(4, new Integer(nParam)));
        m_nParam = nParam;
    }

    JIPParameterUnboundedException()
    {
        super(create(4, null));
    }

    /** Gets the error term associated to this exception
     * @return JIPTerm object associated to this exception
     */
    public JIPTerm getTerm()
    {
        String strTerm = ((m_term != null) ? (m_term.toString()) : ((m_curNode == null) ? ("undefined") : (m_curNode.getGoal().toString())));

        return getTerm("instantiation_error", strTerm);
    }
}
