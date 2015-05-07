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

final class Clause2 extends BuiltIn
{
    private Enumeration  m_enum;
    private boolean m_bFail = false;

    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        PrologObject head = getParam(1);
        PrologObject body = getParam(2);

        PrologObject bd = getRealTerm(body);

        if(!(bd == null) && !(bd instanceof Functor) && !(bd instanceof Atom))
            throw new JIPTypeException(JIPTypeException.CALLABLE, body);

        Clause clause = Clause.getClause(getRealTerm(head), false);

        head = clause.getHead();

        if (m_enum == null)
        {
            if(!(head instanceof Functor))
            	throw new JIPTypeException(JIPTypeException.CALLABLE, head);

            // Search in DB
            final GlobalDB gdb = getJIPEngine().getGlobalDB();

            if(gdb.isSystem((Functor)head) || gdb.isUser((Functor)head) && !gdb.isDynamic(((Functor)head).getName()))
                throw new JIPPermissionException("access", "private_procedure", ((Functor)head).getPredicateIndicator());

            final JIPClausesDatabase db =
                gdb.search((Functor)head, clause.getModuleName());

            if(db == null)
            {
                m_bFail = true;
                return false;
            }

            m_enum = db.clauses();
        }

        boolean bFound = false;
        ConsCell currentRule = null;
        PrologObject head1, body1;

        while(m_enum.hasMoreElements() && !bFound)
        {
            currentRule = (ConsCell)((ConsCell)m_enum.nextElement()).copy(true);

            head1 = currentRule.getHead();
            body1 = currentRule.getTail();

            body1 = getRealTerm(body1);
            if(body1 == null)
                body1 = Atom.TRUE;
            else if(body1 instanceof ConsCell && !(body1 instanceof Functor))
                body1 = ((ConsCell)body1).getHead();

            bFound = new ConsCell(head, body).unify(new ConsCell(head1, body1), varsTbl);
            //bFound = new ConsCell(head, body).unify(new ConsCell(head1, body1), varsTbl);
        }

        return bFound;
    }

    public final boolean hasMoreChoicePoints()
    {
        if(m_bFail)
            return false;

        return m_enum == null ? true : m_enum.hasMoreElements();
    }
}

