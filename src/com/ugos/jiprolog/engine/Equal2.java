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

import java.util.*;

final class Equal2 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        Expression exp1;
        Expression exp2;
        try
        {
            exp1 = Expression.compute(getParam(1));
            exp2 = Expression.compute(getParam(2));
        }
        catch(ClassCastException ex)
        {
             PrologObject arg1 = getRealTerm(getParam(1));

             if(!(arg1 instanceof Expression))
                  throw new JIPTypeException(JIPTypeException.EVALUABLE, arg1);
             else
                 throw new JIPTypeException(JIPTypeException.EVALUABLE, arg1);
        }

        return exp1.getValue() == exp2.getValue();
    }
}
