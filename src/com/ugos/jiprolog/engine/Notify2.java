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

import java.util.*;

class Notify2 extends BuiltIn
{
    public boolean unify(final Hashtable varsTbl)
    {
        PrologObject term = getParam(2);
        Expression eventID;
        try
        {
            eventID = Expression.compute(getRealTerm(getParam(1)));
            term = getParam(2);
        }
        catch(JIPRuntimeException ex)
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.INTEGER);
        }
        
        notifyEvent((int)eventID.getValue(), term.copy());
        
        return true;
    }
        
    final void notifyEvent(final int nID, final PrologObject term)
    {
        getJIPEngine().notifyEvent(nID, term, getQueryHandle());
    }
}
