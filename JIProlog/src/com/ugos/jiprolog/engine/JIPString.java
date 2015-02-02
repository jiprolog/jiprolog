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
 * JIPString wraps to a prolog string.<br>
 * A prolog string is a list af ASCII characters contained in the string.
 * @version 3.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPTerm
 */
public class JIPString extends JIPTerm
{
    private final static long serialVersionUID = 300000001L;

    /** Creates a new JIPString object from a string
      * @param strString JIPString object from a string
      */
    public static final JIPString create(final String strString, boolean atom)
    {
        return new JIPString(new PString(strString, atom));
    }

    /** Creates a new JIPString object from a list of characters
      * @param string JIPList object
      */
    public static final JIPString create(final JIPList string)
    {
        return new JIPString(new PString((List)string.getRealTerm()));
    }

    JIPString(PString str)
    {
        super(str);
    }

    /** Returns the string
      */
    public final String getStringValue()
    {
        return ((PString)getTerm()).getString();
    }
}
