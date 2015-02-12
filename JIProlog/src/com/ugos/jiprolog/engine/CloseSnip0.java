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

final class CloseSnip0 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        // estrae il nodo corrente
        final WAM.Node curNode = getWAM().getCurNode();

        // risale lo stack in cerca dell'opensnip
        WAM.Node previousNode = curNode.m_previous;
        while(previousNode != null)
        {
            final PrologObject goal = previousNode.getGoal();
            if(goal instanceof Functor)
            {
                if(((Functor)goal).getName().equals("<!/0"))
                {
                    // imposta il backtrack alla call corrente
                    curNode.m_backtrack = previousNode.m_previous;
                    return true;
                }
            }

            previousNode = previousNode.m_previous;
        }

        throw JIPRuntimeException.createRuntimeException(19);
    }
}
