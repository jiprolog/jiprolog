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

/**
 * JIPErrorEvent is used to notify an error or an exception occured during execution.<br>
 * JIPEventListener receives a JIPErrorEvent object in the method errorNotified
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPEventListener
 */
public class JIPErrorEvent extends JIPEvent
{
    private String m_strErr;
    private JIPRuntimeException m_ex;

    /**
    * ID of an Error Event
    */
    public static final int ID_ERROR    = -10;

    JIPErrorEvent(final JIPEngine source, final int nHandle, final JIPRuntimeException ex)
    {
        super(ID_ERROR, null, source, nHandle);
//        if(ex.getTerm() != null)
//            m_strErr = ex.getMessage() + "\nRaised by " + ex.getTerm().toString(source);
//        else
            m_strErr = ex.getMessage();

        m_ex = ex;
    }

   /** Gets the error message
     * @return string representation of the error occurred
     */
    public String getError()
    {
        return m_strErr;
    }

    /** Gets the file name where the error was raised
     * @return the file name where the exception was raised
     */
    public String getFileName()
    {
        return m_ex.m_strFileName;
    }

    /** Gets the line number where the error was raised
     * @return the line number where the exception was raised
     */
    public int getLineNumber()
    {
        return m_ex.m_nLineNumber;
    }

    /** Gets the position where the error was raised
     * @return the position where the exception was raised
     */
    public int getPosition()
    {
        return m_ex.m_nPosition;
    }

//    /** Gets the error number
//      * @return number of the error occurred
//      */
//    public int getErrorNumber()
//    {
//        return (m_ex).getErrorNumber();
//    }

    /** Gets the error number
      * @return number of the error occurred
      */
    public JIPRuntimeException getException()
    {
        return m_ex;
    }

   /**
     * Gets a string representation of this JIPErrorEvent object
     * @return string representation of this JIPErrorEvent object
     */
    public String toString()
    {
        return m_strErr;
    }
}
