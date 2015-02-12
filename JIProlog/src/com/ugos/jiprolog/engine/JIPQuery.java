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
//#ifndef _MIDP
import java.io.Serializable;
//#endif

/**
 * JIPQuery wraps a single synchronous query.<br>
 * By <i>asyncronous</i> call it is intended a call to prolog interpreter that runs in a
 * separate thread and notify for solutions all event listeners<br>.
 * By <i>syncronous</i> call it is intended a call to prolog interpreter that runs in the same
 * thread and exit when a solution is found<br>.
 * An asyncronous, more powerful, management of the prolog interpreter see JIPEngine.<br>
 * For more information see the section <i>"How to make a synchronous call to prolog interpreter"</i>
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * @since 2.0
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPEngine
 */
public class JIPQuery extends Object implements Serializable
{
    private final static long serialVersionUID = 300000001L;

    private WAM          m_wam;
    private PrologObject m_query;
    private boolean      m_bOpen;
    private boolean      m_bNoMore;
    private boolean      m_bSoftAbort = false;

    JIPQuery(final PrologObject query, final WAM wam)
    {
        m_wam   = wam;
        m_query = query.copy(true);
    }

    /** Searches for another solution.
     * When a solution is found it returns the corresponding JIPTerm object.
     * If there are no more solutions it returns null.<br>
     * Raises a JIPQueryClosedException if called again after it has returned null.
     * @return next solution found or null if there are no more solution.
     * @exception com.ugos.jiprolog.engine.JIPQueryClosedException
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public final JIPTerm nextSolution() throws JIPQueryClosedException
    {
        boolean bSolution;
        try
        {
	        if(m_bNoMore)
	        {
	            throw JIPRuntimeException.createRuntimeException(49, m_query.toString());
	        }
	        else if (!m_bOpen)
	        {
	            m_bOpen = true;
	            bSolution = m_wam.query(m_query);
	        }
	        else
	        {
	            bSolution = m_wam.nextSolution();
	        }
        }
        catch(JIPAbortException ex)
        {
        	if(m_bSoftAbort)
        		return null;

        	throw ex;
        }
        catch(JIPRuntimeException ex)
        {
        	throw ex;
        }

        // try/catch occorre per intercettare StackOverflow sulla copy
        try
        {
            if(bSolution)
            {
                return JIPTerm.getJIPTerm(m_query.copy(false));
            }
            else
            {
                m_bNoMore = true;
                return null;
            }
        }
        catch(Throwable er)
        {
            throw new JIPJVMException(er);
        }

    }

    /** Returns true if the query has more choice points on backtracking.<br>
     * @return true if the query has more choice points or if nextSolution has not been colled yet, false otherwise.
     */
    public final boolean hasMoreChoicePoints()
    {
        if(m_bNoMore)
            return false;
        else if(m_wam.isNeverRun())
            return true;  // qui è tutto pronto per partire
        else if (m_wam.isClosed())
            return false;

        return m_wam.hasMoreChoicePoints();
    }

    /** Returns true if the query is closed.<br>
     * @return true if the query is closed, false otherwise.
     */
    public final boolean isClosed()
    {
        return m_wam.isClosed();
    }

    /** Closes the query<br>
     */
    public final void close()
    {
        if (!isClosed())
        {
            m_wam.closeQuery();
        }
    }

    /** Called by the interpreter to finalize this object<br>.
     * It closes the query if it isn't closed yet.
     * Developer would not call it directly.
     */
    protected void finalize()
    {
        close();
    }

	public boolean isSoftAbort()
	{
		return m_bSoftAbort;
	}

	public void setSoftAbort(boolean bSoftAbort)
	{
		this.m_bSoftAbort = bSoftAbort;
	}

}

