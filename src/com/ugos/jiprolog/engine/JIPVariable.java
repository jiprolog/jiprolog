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
 * JIPVariable wraps a prolog variable
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPTerm
 */
public class JIPVariable extends JIPTerm
{
    private final static long serialVersionUID = 300000001L;

    /** Creates a new Variable
     * @param strName the name of the variable
     */
    public static final JIPVariable create(final String strName)
    {
        return (strName == null) ?
            new JIPVariable(new Variable(false))
            :
            new JIPVariable(new Variable(strName));
    }

    /** Creates a new anonimous Variable
     */
    public static final JIPVariable create()
    {
        return create(null);
    }

    JIPVariable(final Variable var)
    {
        super(var);
    }

    /** If the variable is bounded it returns the JIPTerm object bounded, otherwise it return null.
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public JIPTerm getValue()
    {
        if(isBounded())
            return JIPTerm.getJIPTerm((((Variable)getTerm()).getObject()));
        else
            return null;
    }

    /** Returns the name of the variable
     */
    public final String getName()
    {
        return ((Variable)getTerm()).getName();
    }

    /** Checks if the variable is bounded (instantiated)
     */
    public final boolean isBounded()
    {
        return ((Variable)getTerm()).isBounded();
    }

    /** Checks if the variable is anonymous
     */
    public final boolean isAnonymous()
    {
        return ((Variable)getTerm()).isAnonymous() || ((Variable)getTerm()).isShadow();
    }
}
