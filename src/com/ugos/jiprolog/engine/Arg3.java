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

final class Arg3 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        final PrologObject place = getRealTerm(getParam(1));
        final PrologObject term = getRealTerm(getParam(2));

        // instantiation errors come first
        if(place == null)
            throw new JIPInstantiationException(1);
        if(term == null)
            throw new JIPInstantiationException(2);
        // then type errors
        if(!(place instanceof Expression))
            throw new JIPTypeException(JIPTypeException.INTEGER, place);
        if(!((Expression)place).isInteger())
            throw new JIPTypeException(JIPTypeException.INTEGER, place);
        if(!(term instanceof ConsCell))
            throw new JIPTypeException(JIPTypeException.COMPOUND, term);

        // and after that domain errors
        if(((Expression)place).getValue() < 0)
            throw new JIPDomainException("not_less_than_zero", place);

        int index = (int)((Expression)place).getValue();

        PrologObject val   = getParam(3);
        PrologObject param = null;

        try
        {
             if(term instanceof Functor)
             {
                 if(index > ((Functor)term).getParams().getHeight())
                     return false;

                 param = ((Functor)term).getParams().getTerm(index);
             }
             else if(term instanceof ConsCell) // [Arg1|Arg2] = '.'(Arg1, Arg2)
             {
                 if(index == 1)
                     param = ((ConsCell)term).getHead();
                 else if(index == 2)
                 {
                     param = ((ConsCell)term).getTail();
                     if(param == null)
                    	 param = List.NIL;
                 }
                 else
                     return false;
            }

            if(param == null)
                return false;

            return param.unify(val, varsTbl); 
        }
        catch (ClassCastException ex)
        {
            throw  new JIPTypeException(JIPTypeException.COMPOUND, term);
        }
    }

}
