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

final class Compare3 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        //System.out.println("compare");
        final PrologObject oper  = getRealTerm(getParam(1));
        final PrologObject term1 = getParam(2);
        final PrologObject term2 = getParam(3);
        
        PrologObject oper1;
        if (oper == null)
        {
            if(term1.lessThen(term2))
                oper1 = Atom.createAtom("<");
            else if(term2.lessThen(term1))
                oper1 = Atom.createAtom(">");
            else
                oper1 = Atom.createAtom("=");
            
            return getParam(1).unify(oper1, varsTbl);
        }
        else
        {
            try
            {
                final char op = ((Atom)oper).getName().charAt(0);
                switch(op)
                {
                    case '<':
                        return term1.lessThen(term2);
                    case '>':
                        return term2.lessThen(term1);
                    case '=':
                        return !term1.lessThen(term2) && !term2.lessThen(term1);
                    default:
                        throw  new JIPParameterTypeException(1, JIPParameterTypeException.COMPARATION_OPERATOR);
                }
            }
            catch(ClassCastException ex)
            {
                //System.out.println(ex.toString());
                throw  new JIPParameterTypeException(1, JIPParameterTypeException.COMPARATION_OPERATOR);
            }
        }
    }
}
