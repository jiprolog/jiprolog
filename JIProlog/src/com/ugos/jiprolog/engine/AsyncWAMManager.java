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

final class AsyncWAMManager implements Runnable
{
    WAM             m_wam;
    PrologObject    m_query;
    Object          m_result;

    private JIPEngine       m_engine;
    private boolean         m_bNext;
    private Thread          m_workerThread;

    //private boolean               m_bDeterministc;

    // il container può trovarsi in due stati:
    // running se m_workerThread != null
    // waiting se m_workerThread == null

    public AsyncWAMManager(final WAM wam, final PrologObject query, JIPEngine engine)
    {
        m_wam    = wam;
        m_query   = query;
        m_engine  = engine;
        m_workerThread    = null;
    }

    public final int getHandle()
    {
        return m_wam.hashCode();
    }

    public final void start()
    {
        m_workerThread = new Thread(this);
        m_workerThread.setDaemon(true);
        m_workerThread.setName("JIProlog engine");
        m_workerThread.start();
    }

    public final void next()
    {
        m_bNext = true;
        start();
    }

    final boolean isRunning()
    {
        return m_workerThread != null;
    }

    final void close()
    {
        m_wam.closeQuery();
    }

    public final void run()
    {
        // call interpreter
        try
        {
            if(m_bNext)
            {
                if(!m_wam.nextSolution())
                    m_result = null;
            }
            else
            {
                final PrologObject query = m_query.copy(false);
                if(m_wam.query(query))
                {
                    m_result = query;
                }
                else
                    m_result = null;
            }
        }
        catch(Throwable th)
        {
        	th.printStackTrace();
            m_result = th;
        }

        // set container state
        m_workerThread = null;

        // update the caller
        m_engine.update(this);
    }

    final boolean hasMoreChoicePoints()
    {
        if(m_wam.isNeverRun())
        {
            return true;  // qui è tutto pronto per partire
        }
        else if (m_wam.isClosed())
        {
            return false;
        }

        return m_wam.hasMoreChoicePoints();
    }
}
