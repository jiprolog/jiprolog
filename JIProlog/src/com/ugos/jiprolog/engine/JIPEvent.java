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
 * JIPEvent is used to notify an interpreter event occurred during the execution<br>
 * JIPEventListeners receive a JIPEvent object in one of their methods depending on the type of event<br>
 * @version 1.8.x.x
 * @author Ugo Chirico 2000<br>
 * Home Page : http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPEventListener
 * @see com.ugos.jiprolog.engine.JIPEventListener#openNotified
 * @see com.ugos.jiprolog.engine.JIPEventListener#closeNotified
 * @see com.ugos.jiprolog.engine.JIPEventListener#moreNotified
 * @see com.ugos.jiprolog.engine.JIPEventListener#endNotified
 * @see com.ugos.jiprolog.engine.JIPEventListener#solutionNotified
 * @see com.ugos.jiprolog.engine.JIPEventListener#termNotified
 */
public class JIPEvent extends Object
{
    private int              m_nID;
    private PrologObject     m_term;
    private JIPEngine        m_source;
    private int              m_nQueryHandle;
    private long             m_when;
    private boolean          m_bConsumed = false;
 
    /**
    * ID of a Solution Event
    */
    public static final int ID_SOLUTION = -1;

    /**
    * ID of an Open Event
    */
    public static final int ID_OPEN    = -2;
    
    /**
    * ID of a Close Event
    */
    public static final int ID_CLOSE    = -3;

    /**
    * ID of a End Event
    */
    public static final int ID_END    = -4;
    
    /**
    * ID of a More Event
    */
    public static final int ID_MORE    = -5;
    
    /**
    * ID of a WaitForUserInput Event
    */
    public static final int ID_WAITFORUSERINPUT    = -6;
    
    /**
    * ID of a WaitForUserInput Event
    */
    public static final int ID_USERINPUTDONE    = -7;
    
    /**
    * ID of a Undefined predicate Event
    */
    public static final int ID_UNDEFPREDICATE    = -8;
    
    /**
    * ID of a Sigleton Variables Found Event
    */
    public static final int ID_SINGLETONVARS     = -9;

    /**
     * ID of a encoding changed Event
     */
     public static final int ID_ENCODINGCHANGED     = -10;

    
      
    JIPEvent(int nID, PrologObject term, JIPEngine source, int nQueryHandle)
    {
        m_when   = System.currentTimeMillis();
        m_term   = term;
        m_nID    = nID;
        m_source = source;
        m_nQueryHandle = nQueryHandle;
        m_bConsumed = false;
    }

   /**
     * Gets the ID of this JIPEvent object
     * @return ID of the event
    */
    public int getID()
    {
        return m_nID;
    }

    /**
     * Gets the handle of the query related to this JIPEvent object
     * @return handle of the query
     */
     public int getQueryHandle()
     {
         return m_nQueryHandle;
     }
   
   /**
     * Timestamp of this JIPEvent object
     * @return timestamp of this JIPEvent object
    */
    public long when()
    {
        return m_when;
    }
    
    /**
     * Gets the source of this JIPEvent object
     * @return instance of JIPEngine object sending this JIPEvent object
     * @see com.ugos.jiprolog.engine.JIPEngine
    */
    public JIPEngine getSource()
    {
        return m_source;
    }
    
    /**
     * Gets the notified term
     * @return term notified by user (notify/2)
     * @see com.ugos.jiprolog.engine.JIPTerm
    */
    public JIPTerm getTerm()
    {
        if(m_term != null)
            return JIPTerm.getJIPTerm(m_term);
        else
            return null;
    }
    
    /**
    * Consumes this JIPEvent object so other JIPEventListeners don't receive it
    */
    public void consume()
    {
        m_bConsumed = true;
    }
    
    boolean consumed()
    {
        return m_bConsumed;
    }
}
