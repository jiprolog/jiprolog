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

import java.util.*;

final class TermEquals2 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
//    	String s1 = getParam(1).toString();
//    	String s2 = getParam(2).toString();

//    	System.out.println(s1);
//    	return getParam(1).unifiable(getParam(2));


    	String s1 = getParam(1).toString();
    	String s2 = getParam(2).toString();
        return s1.equals(s2);
    }
}
