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
            throw new JIPParameterUnboundedException(1);
        if(term == null)
            throw new JIPParameterUnboundedException(2);
        // then type errors
        if(!(place instanceof Expression))
            throw new JIPTypeException(JIPTypeException.INTEGER, place);
        if(!((Expression)place).isInteger())
            throw new JIPTypeException(JIPTypeException.INTEGER, place);
        if(!(term instanceof Functor) && !(term instanceof List))
            throw new JIPTypeException(JIPTypeException.COMPOUND, term);

        // and after that domain errors
        if(((Expression)place).getValue() < 0)
            throw new JIPDomainException("not_less_than_zero", place);

    	int index = (int)((Expression)place).getValue();

        PrologObject val   = getParam(3);
        PrologObject param = null;

        try
        {

            if(term instanceof List) // [Arg1|Arg2] = '.'(Arg1, Arg2)
            {
//				if(((Expression)place).getValue() == 1)
//					param = (List)term)
//					param = new ConsCell(((List)term).getHead();
//				else if(((Expression)place).getValue() == 2)
//					param = new ConsCell(((List)term).getTail();

            	if(index > ((Functor)term).getParams().getHeight())
            		return false;

            	param = ((List)term).getTerm(index);

//                param = new ConsCell(((List)term).getHead(), new ConsCell(((List)term).getTail(), null)).getTerm();
            }
            else if(term instanceof Functor)
            {
            	if(index > ((Functor)term).getParams().getHeight())
            		return false;

                param = ((Functor)term).getParams().getTerm(index);
            }
            else // ConsCell
            {
            	if(index > ((Functor)term).getParams().getHeight())
            		return false;

                param = ((ConsCell)term).getTerm(index);
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
