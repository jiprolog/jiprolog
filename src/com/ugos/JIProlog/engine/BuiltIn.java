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

package com.ugos.JIProlog.engine;

import java.util.Hashtable;


abstract class BuiltIn extends Object
{
    private ConsCell           m_params            = null;
    private WAM                m_WAM;
    protected JIPEngine        m_jipEngine;
    protected BuiltInPredicate m_predicate;

    public abstract boolean unify(final Hashtable<Variable, Variable> m_varsTbl);

    final boolean unify(final ConsCell param, final Hashtable<Variable, Variable> varsTbl)
    {
        m_params = param;
        return unify(varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }

    final public WAM getWAM()
    {
        return m_WAM;
    }

    // chiamato solo da BuiltInFactory
    final void init(final JIPEngine jipEngine, final BuiltInPredicate pred, final WAM wam)
    {
        m_jipEngine = jipEngine;
        m_predicate = pred;
        m_WAM = wam;
    }

    final public int getQueryHandle()
    {
        return m_WAM.hashCode();
    }

    final ConsCell getParams()
    {
        return m_params;
    }

    final PrologObject getParam(final int n)
    {
        return m_params.getTerm(n);
    }

    final JIPEngine getJIPEngine()
    {
        return m_jipEngine;
    }

    final BuiltInPredicate getPredicate()
    {
        return m_predicate;
    }

    static final PrologObject getRealTerm(final PrologObject term)
    {
        if(term instanceof Variable)
            return ((Variable)term).getObject();

        return term;
    }
}
