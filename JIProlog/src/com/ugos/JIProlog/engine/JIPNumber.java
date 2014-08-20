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

package com.ugos.JIProlog.engine;

/**
 * JIPNumber wraps a prolog number
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.JIProlog.engine.JIPTerm
 */
public class JIPNumber extends JIPTerm
{
    private final static long serialVersionUID = 300000001L;
    
    /** Creates a new JIPNumber object
     * @param d the number
     * @return a new JIPNumber object
      */
    public static final JIPNumber create(final double d)
    {
        return new JIPNumber(Expression.createNumber(d));
    }

    /** Creates a new JIPNumber object
     * @param n the number
     * @return a new JIPNumber object
      */
    public static final JIPNumber create(final int n)
    {
        return new JIPNumber(Expression.createNumber(n));
    }
    
    JIPNumber(Expression exp)
    {
        super(exp);
    }

    /** Returns the numeric value of this JIPNumber object
     * @return numeric value of this JIPNumber object
     */
    public final double getValue()
    {
        return ((Expression)getTerm()).getValue();
    }
    
    public final boolean isInteger()
    {
        return ((Expression)getTerm()).isInteger();
    }
}
