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

import java.util.Hashtable;

final class Length2 extends BuiltIn
{
    public final boolean unify(final Hashtable varsTbl)
    {
        final PrologObject pred = getRealTerm(getParam(1));

        final PrologObject length = getRealTerm(getParam(2));

        if(length != null)
        {
            if(!(length instanceof Expression))
            {
            	throw new JIPTypeException(JIPParameterTypeException.INTEGER, length);
            }
            else if(!((Expression)length).isInteger())
        	{
        		throw new JIPTypeException(JIPParameterTypeException.INTEGER, length);
        	}
        	else if(((Expression)length).getValue() < 0)
        	{
        		throw new JIPDomainException("not_less_than_zero", length);
        	}
        }

        if(pred instanceof ConsCell)
        {
            return Expression.createNumber(((ConsCell)pred).getHeight()).unify(getParam(2), varsTbl);
        }
        else if(pred instanceof Atom)
        {
            return Expression.createNumber(((Atom)pred).getName().toString().length()).unify(getParam(2), varsTbl);
        }
        else if(pred instanceof Expression)
        {
        	String s = pred.toString();
        	return Expression.createNumber(s.length()).unify(getParam(2), varsTbl);
//            throw new JIPParameterTypeException(1, JIPParameterTypeException.INTEGER);
        }
        else
        {
            List list = null;
            for(int i = 0; i < (int)((Expression)length).getValue(); i++)
            {
                list = new List(new Variable(false), list);
            }

            return getParam(1).unify(list, varsTbl);
        }
    }


}

