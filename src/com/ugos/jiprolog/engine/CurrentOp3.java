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

final class CurrentOp3 extends BuiltIn
{
    private Operator    m_curOp = null;
    private Operator    m_supOp = null;
    private Enumeration m_enum  = null;
    private ConsCell    m_second = null;

    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        PrologObject prec  = null;
        PrologObject assoc = null;
        PrologObject op    = null;

        if (m_enum == null)
        {
            prec  = getParam(1);
            assoc = getParam(2);
            op    = getParam(3);

            PrologObject test = getRealTerm(prec);
            if(test != null)
            {
            	if(!(test instanceof Expression))
            		throw new JIPTypeException(JIPTypeException.INTEGER, prec);
            	else if(!((Expression)test).isInteger())
            		throw new JIPTypeException(JIPTypeException.INTEGER, prec);
            	else if(((Expression)test).getValue() < 0 || ((Expression)test).getValue() > 1200)
            		 throw new JIPDomainException("operator_priority", prec);
            }

            test = getRealTerm(assoc);
            if(test != null)
            {
            	if(!(test instanceof Atom))
            		throw new JIPTypeException(JIPTypeException.ATOM, assoc);
            	else if(!"fx, fy, xfx, xfy, yfx, xf, yf".contains(((Atom)test).getName()))
            		throw new JIPDomainException("operator_specifier", assoc);
            }

            test = getRealTerm(op);
            if(test != null)
            {
            	if(!(test instanceof Atom))
            		 throw new JIPTypeException(JIPTypeException.ATOM, op);
            }

            m_second =
                new ConsCell(prec,
                             new ConsCell(assoc,
                                          new ConsCell(op, null)));

            m_enum = getJIPEngine().getOperatorManager().getOperators();
        }

        if(m_supOp != null)
        {
            if(m_supOp != null)
            {
                final ConsCell first =
                    new ConsCell(Expression.createNumber(m_supOp.getPrecedence()),
                                 new ConsCell(Atom.createAtom(m_supOp.getAssoc()),
                                              new ConsCell(Atom.createAtom(m_supOp.getName()), null)));

                if(first.unify(m_second, varsTbl))
                {
                    m_curOp = m_supOp;
                    m_supOp = null;
                    return true;
                }
            }
        }

        while(m_enum.hasMoreElements())
        {
            m_curOp = (Operator)m_enum.nextElement();

//          System.out.println(m_curOp);
//            System.out.println(m_curOp.getName());
            final ConsCell first =
                new ConsCell(Expression.createNumber(m_curOp.getPrecedence()),
                             new ConsCell(Atom.createAtom(m_curOp.getAssoc()),
                                          new ConsCell(Atom.createAtom(m_curOp.getName()), null)));

            if(first.unify(m_second, varsTbl))
            {
                m_supOp = m_curOp.getSupplementaryOp();
                return true;
            }
        }

        return false;
    }

    public final boolean hasMoreChoicePoints()
    {
        return m_enum == null ? true : m_enum.hasMoreElements();
    }
}
