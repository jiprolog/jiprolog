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

/**
 * JIPTraceEvent is used to notify a thar a Trace event occured.
 * Note that trace events are raised only when trace flag is set to true.<br>
 * JIPTraceListeners receive a JIPTraceEvent object in one of their methods depending on the type of event occurred
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPTraceListener
 * @see com.ugos.jiprolog.engine.JIPTraceListener#startNotified
 * @see com.ugos.jiprolog.engine.JIPTraceListener#callNotified
 * @see com.ugos.jiprolog.engine.JIPTraceListener#foundNotified
 * @see com.ugos.jiprolog.engine.JIPTraceListener#bindNotified
 * @see com.ugos.jiprolog.engine.JIPTraceListener#exitNotified
 * @see com.ugos.jiprolog.engine.JIPTraceListener#failNotified
 * @see com.ugos.jiprolog.engine.JIPTraceListener#redoNotified
 * @see com.ugos.jiprolog.engine.JIPTraceListener#stopNotified
 */
public class JIPTraceEvent extends JIPEvent
{
    private WAMTrace m_wam;

    /**
    * ID of a Call event
    */
    public static final int ID_CALL  = -20;

    /**
    * ID of a Bind event
    */
    public static final int ID_BIND = -21;
    
    /**
    * ID of a Found event
    */
    public static final int ID_FOUND = -22;
    
    /**
    * ID of a Redo event
    */
    public static final int ID_REDO  = -23;

    /**
    * ID of a Fail event
    */
    public static final int ID_FAIL  = -24;
    
    /**
    * ID of a Exit event
    */
    public static final int ID_EXIT  = -25;
    
    /**
    * ID of a Start event
    */
    public static final int ID_START  = -26;

    /**
    * ID of a Stop event
    */
    public static final int ID_STOP  = -27;
    
    
    private static final int ABORT   = 1;
    private static final int NEXT    = 2;
    private static final int SKIP    = 3;
    private static final int RETRY   = 4;
    
    private int m_nToDo;
    private int m_nLevel;
    
    JIPTraceEvent(final int nID, final PrologObject term, final JIPEngine source, final int nQueryHandle, final WAMTrace wam, final int nLevel)
    {
        super(nID, term, source, nQueryHandle);
        m_wam = wam;
        m_nLevel = nLevel;
    }

   /**
    * Gets the trace level
    */
    public int getLevel()
    {
        return m_nLevel;
    }

   /**
    * Aborts the execution
    */
    public void abort()
    {
       m_nToDo = ABORT;
       go();
    }

   /**
    * Executes next step
    */
    public void nextStep()
    {
       m_nToDo = NEXT;
       go();
    }
    
    /**
    * Retry current call
    */
    public void retry()
    {
       m_nToDo = RETRY;
       go();
    }
    
   /**
    * Skips the trace for next call
    */
    public void skip()
    {
        m_nToDo = SKIP;
        go();
    }
    
    boolean executionAborted()
    {
        return m_nToDo == ABORT;
    }
    
    boolean retryCall()
    {
        return m_nToDo == RETRY;
    }
    
    boolean skipped()
    {
        return m_nToDo == SKIP;
    }
    
    private void go()
    {
        m_wam.notifyUserInput();
    }
    
}
