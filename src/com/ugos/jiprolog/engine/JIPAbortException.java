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
public class JIPAbortException extends JIPRuntimeException
{
    /**
	 *
	 */
	private static final long	serialVersionUID	= -6669335200974896775L;

	/** Constructs a new JIPAbortException
     */
    public JIPAbortException()
    {

    }

    /** Gets the error term associated to this exception
     * @return JIPTerm object associated to this exception
     */
    public JIPTerm getTerm()
    {
    	return getTerm(Atom.createAtom("execution_aborted"));
    }
}
