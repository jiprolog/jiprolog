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

import java.util.Hashtable;

class Functor extends ConsCell
{
    final static long serialVersionUID = 300000005L;

    private String   m_strName;
    private int      m_nArity;
    private String   m_strFriendlyName;

    public Functor(final String strName, final ConsCell params)
    {
        this(Atom.createAtom(strName), params);
    }

    public Functor(final Atom name, final ConsCell params)
    {
        super(name, params);

        m_strName    = name.getName();

        final int nPos = m_strName.lastIndexOf('/');
        if(nPos == -1)
        {
            m_nArity = 0;
            m_strFriendlyName = m_strName;
        }
        else
        {
//          System.out.println(m_strName);
            m_nArity = Integer.parseInt(m_strName.substring(nPos + 1, m_strName.length()));
            m_strFriendlyName = m_strName.substring(0, nPos);
        }
    }

    public Functor(final Atom name)
    {
        super(name, null);

        m_strName    = name.getName();

        m_nArity = 0;
        m_strFriendlyName = m_strName;
    }

    public PrologObject copy(final boolean flat, final Hashtable<Variable, PrologObject> varTable)
    {
        if(getParams() != null)
            return new Functor(m_strName, (ConsCell)(getParams().copy(flat, varTable)));
        else
            return new Functor(m_strName, null);
    }

    public final String getName()
    {
        return m_strName;
    }

    public final Atom getAtom()
    {
        return (Atom)m_head;
    }

    public final String getFriendlyName()
    {
        return m_strFriendlyName;
    }

    public final int getArity()
    {
        return m_nArity;
    }

    public final ConsCell getParams()
    {
        return (ConsCell)m_tail;
    }

    public final void setParams(final ConsCell params)
    {
        m_tail = params;
    }

    public boolean _unify(final PrologObject obj, final Hashtable<Variable, Variable> table)
    {
//      System.out.println("*** Functor unify: " + this + " - " + obj);
        //System.out.println(obj);
        //System.out.println(obj.getClass());

        if (obj instanceof Functor)
        {
//          System.out.println("*** obj is Functor: ");
            return m_head._unify(((Functor)obj).m_head, table) &&
                  ((m_tail == null) ? (((Functor)obj).m_tail == null) : (m_tail._unify(((Functor)obj).m_tail, table)));
        }
        else if(obj instanceof Variable)
        {
            return obj._unify(this, table);
        }
        else
        {
            return false;
        }
    }

    static final Functor getFunctor(PrologObject term)
    {
        // check if variable (used in metacall variable
        if (term instanceof Variable)
        {
            term = ((Variable)term).getObject();
        }

        // check type
        if (term instanceof Atom)
        {
            if(BuiltInFactory.isBuiltIn(((Atom)term).getName() + "/0"))
            {
                term = new BuiltInPredicate(((Atom)term).getName() + "/0", null);
            }
            else
            {
                term = new Functor(((Atom)term).getName() + "/0", null);
            }
        }

        if(term instanceof Functor)
            return (Functor)term;
        else
            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, term.toString());

    }
}
