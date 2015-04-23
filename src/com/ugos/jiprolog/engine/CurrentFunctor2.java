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

final class CurrentFunctor2 extends BuiltIn
{
    private Enumeration m_enum = null;
    private boolean m_bSystem = true;

    @Override
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        PrologObject paramName = getRealTerm(getParam(1));
        PrologObject paramArity = getRealTerm(getParam(2));

        if(paramName != null && !((paramName instanceof Functor) || paramName instanceof Atom))
         throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, new Functor("//2", new ConsCell(paramName, new ConsCell(paramArity, null))));

        if(paramArity != null && !(paramArity instanceof Expression))
            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, new Functor("//2", new ConsCell(paramName, new ConsCell(paramArity, null))));

        if(m_enum == null)
        {
            m_enum = getJIPEngine().getGlobalDB().databases();
            m_bSystem = true;
        }

        Atom funcName;
        Expression arity;

        if(m_bSystem)
        {
            while(m_enum.hasMoreElements())
            {
                JIPClausesDatabase db = (JIPClausesDatabase)m_enum.nextElement();

                funcName = Atom.createAtom(db.getFunctorName());
                arity    = Expression.createNumber(db.getArity());

                if(getParam(1).unifiable(funcName) &&
                   getParam(2).unifiable(arity))
                {
                    if(!m_enum.hasMoreElements())
                    {
                        m_enum = BuiltInFactory.m_BuiltInTable.keys();
                        m_bSystem = false;
                    }

                    return getParam(1).unify(funcName, varsTbl) &&
                           getParam(2).unify(arity, varsTbl);

//                    return true;
                }
            }

            m_bSystem = false;
            m_enum = BuiltInFactory.m_BuiltInTable.keys();

            return unify(varsTbl);
        }
        else
        {
            while(m_enum.hasMoreElements())
            {
                final String strPredDef = (String)m_enum.nextElement();

                int nPos = strPredDef.lastIndexOf('/');

                funcName = Atom.createAtom(strPredDef.substring(0, nPos));
                arity    = Expression.createNumber(Integer.parseInt(strPredDef.substring(nPos + 1)));

                if(getParam(1).unify(funcName, varsTbl) &&
                   getParam(2).unify(arity, varsTbl))
                {
                    return true;
                }
            }

            return false;
        }
    }

    @Override
    public final boolean hasMoreChoicePoints()
    {
        return m_enum == null ? true : m_enum.hasMoreElements();
    }
}
