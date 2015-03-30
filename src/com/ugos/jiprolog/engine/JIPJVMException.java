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
 * JIPJVMException is the exception raised by the interpreter when a Java generic exception
 * is raised during execution.
 * @version 3.0
 * @author Ugo Chirico 2005<br>
 * Home Page: http://www.ugochirico.com
 */
public class JIPJVMException extends JIPRuntimeException
{
    private Throwable m_ex;

    /** Constructs a new JIPJVMException
     * @param ex the JVM exception thrown
     */
    public JIPJVMException(final Throwable ex)
    {
        m_ex = ex;
    }

    public Throwable getInnerException()
    {
    	return m_ex;
    }

    /** Gets the error term associated to this exception
     * @return JIPTerm object associated to this exceptstrMessageXion
     */
    public JIPTerm getTerm()
    {
        String strMessage = m_ex.getMessage();
        if(strMessage == null)
            strMessage = m_ex.toString();

    	return getTerm(new Functor("jvm_error/1", new ConsCell (Atom.createAtom(strMessage), null)));

//        return getTerm("jvm_error(" + strMessage + ")", strTerm);
    }
}
