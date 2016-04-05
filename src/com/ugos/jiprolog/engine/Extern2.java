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

final class Extern2 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        String       strFunctName;
        String       strModuleName;
        String       strXClassName;
        ConsCell     params;
        int 		 nArity;
        PrologObject pred = getRealTerm(getParam(1));

        // controlla se identificativo di modulo
        if(pred instanceof Functor && ((Functor)pred).getAtom().equals(Atom.COLON))
        {
            params = ((Functor )pred).getParams();
            strModuleName = ((Atom)params.getHead()).getName();
            pred = ((ConsCell)params.getTail()).getHead();
        }
        else
        {
            strModuleName = getWAM().m_curNode.m_strModule;
        }

        // head deve essere instanza di funtore /2 del tipo name/arity
        if(pred instanceof Functor && ((Functor)pred).getAtom().equals(Atom.SLASHSLASH))
        {
            params = ((Functor )pred).getParams();
            strFunctName = ((Atom)params.getHead()).getName();
            nArity = (int)((Expression)((ConsCell)params.getTail()).getHead()).getValue();
        }
        else
        {
            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, pred);
        }

        final PrologObject exClass = getRealTerm(getParam(2));

        if(exClass instanceof PString)
            strXClassName = ((PString)exClass).getString();
        else if(exClass instanceof Atom)
            strXClassName = ((Atom)exClass).getName();
        else
            throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, exClass);


        apply(strFunctName, nArity, strModuleName, strXClassName);

        return true;
    }

    protected final void apply(String strFunctName, int nArity, String strModuleName, String strXClassName)
    {
    	BuiltInFactory.addExternalPredicate(strFunctName + "/" + nArity, strModuleName, strXClassName);
    }
}
