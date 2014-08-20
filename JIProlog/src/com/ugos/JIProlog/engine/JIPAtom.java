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

package com.ugos.JIProlog.engine;

/**
 * JIPAtom wraps a prolog atom
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page : http://www.ugochirico.com
 * @see com.ugos.JIProlog.engine.JIPTerm
 */
public class JIPAtom extends JIPTerm implements Clearable
{
    private final static long serialVersionUID = 300000001L;
    
    /** Creates a new JIPAtom object from a string
     * @param strName the atom as string
     * @return a new JIPAtom object from a string
     */
    public static final JIPAtom create(final String strName)
    {
        return new JIPAtom(Atom.createAtom(strName));
    }
    
    JIPAtom(final Atom atom)
    {
        super(atom);
    }
    
    /** Returns a string rapresentation of this atom
      */
    public final String getName()
    {
        return ((Atom)getTerm()).getName();
    }
}
