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
 * JIPSyntaxErrorException is an exception raised by the internal parser
 * when there is a syntax error in the string to parse.<br>
 * @version 3.0
 * @author Ugo Chirico 2004<br>
 * Home Page : http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPEngine
 */
public class JIPSyntaxErrorException extends JIPRuntimeException
{
    private String m_strFile;
    private int m_nLine;
    private String m_strTerm;

    JIPSyntaxErrorException(final String strFileName, final int nLineNumber, final String strTerm)
    {
//        super(create(1, "File: "+ strFileName + ", Line: " + nLineNumber + ", Cause: " + strTerm));
        m_strFile = strFileName;
        m_nLine = nLineNumber;
        m_strTerm = strTerm;
    }

//    JIPSyntaxErrorException(final String msg)
//    {
//        super(create(1, msg));
//    }

    /** Gets the error term associated to this exception
     * @return JIPTerm object associated to this exception
     */
    public JIPTerm getTerm()
    {
        if(m_engine != null)
        {
            String strFile = Atom.createAtom(m_strFile).toString();
	        String strTerm = Atom.createAtom(m_strTerm).toString();
	        try
	        {
	            return m_engine.getTermParser().parseTerm("error(syntax_error(" + strTerm + "), file(" + strFile + ", " + m_nLine + "))");
	        }
	        catch (JIPSyntaxErrorException ex)
	        {
	             return m_engine.getTermParser().parseTerm("error(syntax_error(undefined), file(undefined, undefined))");
	        }
        }
        else
        {
            return super.getTerm();
        }
    }
}
