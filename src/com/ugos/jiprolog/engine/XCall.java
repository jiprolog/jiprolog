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

class XCall extends BuiltIn
{
	private JIPXCall xcall;

	public XCall(String className)
	{
        // Create an instance of JIPXCall class
        xcall = XCall2.createXCall(className);
	}

    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
    	JIPCons exParams = new JIPCons(getParams());

        Hashtable<JIPVariable, JIPVariable> jipVarsTable = new Hashtable<JIPVariable, JIPVariable>();

        // Invoke JIPXCall class
        boolean unify = xcall.unify(exParams, jipVarsTable);

        if(unify)
        {
        	Variable var;
        	for(JIPVariable jvar : jipVarsTable.values())
        	{
        		var = (Variable)jvar.getTerm();
        		varsTbl.put(var, var);
        	}
        }

        return unify;
    }

    public final boolean hasMoreChoicePoints()
    {
    	return xcall.hasMoreChoicePoints();
    }

    @Override
    final void init(final JIPEngine jipEngine, final BuiltInPredicate pred, final WAM wam)
    {
    	super.init(jipEngine, pred, wam);
        xcall.init(this);
    }
}
