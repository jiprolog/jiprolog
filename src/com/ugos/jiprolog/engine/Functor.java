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

import java.util.Enumeration;
import java.util.Hashtable;

import com.ugos.jiprolog.engine.WAM.Node;

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
            m_nArity = Integer.parseInt(m_strName.substring(nPos + 1, m_strName.length()));
            m_strFriendlyName = m_strName.substring(0, nPos);
        }
    }

    public Functor(final Atom name)
    {
        super(name, null);

        m_strFriendlyName    = name.getName();

        m_nArity = 0;
        m_strName = new StringBuilder(m_strFriendlyName).append("/0").toString();// + 0;

        m_head = Atom.createAtom(m_strName);
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

        m_nArity = params.getHeight();
        m_strName = new StringBuilder(m_strFriendlyName).append('/').append(m_nArity).toString();
        m_head = Atom.createAtom(m_strName);
    }

    public boolean _unify(PrologObject obj, final Hashtable<Variable, Variable> table)
    {
    	if(obj instanceof Variable)
        {
        	if(((Variable)obj).isBounded())
        		obj = ((Variable)obj).getObject();
        	else
        		return ((Variable)obj)._unify(this, table);
        }

        if (obj instanceof Functor)
        {
            return m_head._unify(((Functor)obj).m_head, table) &&
                  ((m_tail == null) ? (((Functor)obj).m_tail == null) : (m_tail._unify(((Functor)obj).m_tail, table)));
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
            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, term);

    }

    protected boolean lessThen(PrologObject obj)
    {
    	if(obj instanceof Variable)
        {
            if(((Variable)obj).isBounded())
                obj = ((Variable)obj).getObject();
            else
            	return false;
        }

        if(obj instanceof Functor)
        {
        	int h1 = getArity();
        	int h2 = ((Functor)obj).getArity();
        	if(h1 < h2)
        	{
        		return true;
        	}
        	else if(h1 == h2)
        	{        		
        		if(m_strFriendlyName.compareTo(((Functor) obj).m_strFriendlyName) < 0)
	            {
	            	return true;
	            }
	            else if (m_strFriendlyName.equals(((Functor) obj).m_strFriendlyName))
	            {
		            if(m_tail != null)
		            {
		            	return ((ConsCell)obj).m_tail != null && m_tail.lessThen(((ConsCell)obj).m_tail);
		            }
		            else
		            {
		            	return (((ConsCell)obj).m_tail != null);
		            }
	            }
        	}
        }
        
        return false;
    }
    
    @Override
    public boolean termEquals(PrologObject obj)
    {
    	if(obj instanceof Variable)
        {
            if(((Variable)obj).isBounded())
                obj = ((Variable)obj).getObject();
            else
            	return false;
        }

        if(obj instanceof Functor)
        {
        	int h1 = getArity();
        	int h2 = ((Functor)obj).getArity();
        	if(h1 != h2)
        	{
        		return false;
        	}
        	else 
        	{        		
        		if (m_strFriendlyName.equals(((Functor) obj).m_strFriendlyName))
	            {
		            if(m_tail != null)
		            {
		            	return ((ConsCell)obj).m_tail != null && m_tail.termEquals(((ConsCell)obj).m_tail);
		            }
		            else
		            {
		            	return (((ConsCell)obj).m_tail == null || (((ConsCell)obj).m_tail.termEquals(List.NIL)));
		            }
	            }        		
        	}
        }
        
        return false;
    }
    
    public Functor getPredicateIndicator()
    {
    	return getPredicateIndicator(getName());
    }

    public static Functor getPredicateIndicator(String predicateIndicator)
    {
        int nArity = 0;
        String friendlyName = predicateIndicator;

        int nPos = predicateIndicator.lastIndexOf('/');
        if(nPos > -1)
        {
            nArity = Integer.parseInt(predicateIndicator.substring(nPos + 1, predicateIndicator.length()));
            friendlyName = predicateIndicator.substring(0, nPos);
        }

        return new Functor(Atom.SLASHSLASH, new ConsCell(Atom.createAtom(friendlyName), new ConsCell(Expression.createNumber(nArity), null)));

    }

	@Override
	public Enumeration<PrologRule> getRulesEnumeration(Node curNode, WAM wam)
	{
		// controlla se si tratta di :
        if(m_head.equals(Atom.COLON))
        {
        	curNode.m_strModule = ((Atom)getParams().getHead()).getName();
            PrologObject term = ((ConsCell)getParams().getTail()).getHead();
            term = Functor.getFunctor(term);

            curNode.m_callList.setHead(term);

            return term.getRulesEnumeration(curNode, wam);
        }

        wam.moduleStack.push(curNode.m_strModule);

        return new RulesEnumeration(this, wam.moduleStack, wam.m_globalDB);
	}
}
