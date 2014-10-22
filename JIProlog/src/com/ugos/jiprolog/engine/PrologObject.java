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

import java.util.Enumeration;
import java.util.Hashtable;

//#ifndef _MIDP
import java.io.Serializable;
//#endif

abstract class PrologObject implements Clearable, Serializable
{
    final static long serialVersionUID = 300000001L;

    public final boolean unifiable(final PrologObject obj)
    {
        Hashtable<Variable, Variable> vartbl = new Hashtable<Variable, Variable>(10);
        boolean bUnify = _unify(obj, vartbl);

        Enumeration<Variable> en = vartbl.elements();
        while(en.hasMoreElements())
        {
            en.nextElement().clear();
        }

        return bUnify;
    }

    public final boolean unify(final PrologObject obj, final Hashtable<Variable, Variable> varTbl)
    {
//        System.out.println(toString() + " == " + obj.toString());
//        System.out.println(getClass().toString() + " == " + obj.getClass().toString());

        final Hashtable<Variable, Variable> _varTbl = new Hashtable<Variable, Variable>(10);
        Enumeration<Variable> en;
        if(_unify(obj, _varTbl))
        {
            // riporta le variabili instanziate nella vartable
            Variable var;
            en = _varTbl.elements();
            while(en.hasMoreElements())
            {
                var = en.nextElement();
                varTbl.put(var, var);
            }

            return true;
        }
        else
        {
            // ripulisce le variabili eventualmente instanziate
            en = _varTbl.elements();
            while(en.hasMoreElements())
                ((Clearable)en.nextElement()).clear();

            return false;
        }
    }

    public final String toString()
    {
        return PrettyPrinter.print(this, null, true);
    }

    public final String toString(final JIPEngine engine)
    {
        return PrettyPrinter.print(this, engine.getOperatorManager(),false);
    }

    public final String toStringq(final JIPEngine engine)
    {
        return PrettyPrinter.print(this, engine.getOperatorManager(),true);
    }

    final String toString(final OperatorManager opMan)
    {
        return PrettyPrinter.print(this, opMan, false);
    }

    public final PrologObject copy()
    {
        return copy(new Hashtable(10));
    }

    public abstract void clear();
    public abstract PrologObject copy(Hashtable<Variable, Variable> varTable);
    protected abstract boolean lessThen(PrologObject obj);
    protected abstract boolean _unify(PrologObject obj, Hashtable<Variable, Variable> varTbl);
}

























