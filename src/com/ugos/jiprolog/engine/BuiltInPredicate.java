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

final class BuiltInPredicate extends Functor
{
    private final static long serialVersionUID = 300000001L;

    private BuiltIn     m_builtIn;

    public BuiltInPredicate (final String strName, final ConsCell params)
    {
        super(strName, params);
    }

    public BuiltInPredicate(final Atom name, final ConsCell params)
    {
        this(name.getName(), params);
    }

    public BuiltInPredicate(final Functor functor)
    {
        this(functor.getName(), functor.getParams());
    }

    public final PrologObject copy(final boolean flat, final Hashtable<Variable, PrologObject> varTable)
    {
        BuiltInPredicate bpred;

        if(getParams() == null)
        {
            bpred = new BuiltInPredicate(getName(), null);
        }
        else
        {
            bpred = new BuiltInPredicate(getName(), (ConsCell)(getParams().copy(true, varTable)));
        }

        return bpred;
    }

    public final void init(final WAM wam)
    {
        m_builtIn = wam.getJIPEngine().getBuiltInFactory().getInstance(getName(), this, wam);
    }

    public final void deinit()
    {
        m_builtIn = null;
    }

    public final boolean _unify(final PrologObject obj, final Hashtable<Variable, Variable> table)
    {
        if (m_builtIn != null)
        {
            return m_builtIn.unify(getParams(), table);
        }
        else
        {
            return super._unify(obj, table);
        }
    }

    public final boolean hasMoreChoicePoints()
    {
        if(m_builtIn != null && m_builtIn.hasMoreChoicePoints())
        {
            return true;
        }
        else
        {
            m_builtIn = null;
            return false;
        }
    }
}
