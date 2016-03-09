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

import java.io.*;
//#endif
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * JIPTerm wraps generic prolog terms (atoms, functors, lists, etc.)
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 */
public class JIPTerm extends Object implements Clearable, Serializable, Cloneable
{
    private final static long serialVersionUID = 300000001L;

    private PrologObject m_obj;
    private Vector       m_varsVect;

    /** Checks if this JIPTerm object unifies with the specified term.
     * Remarks: this method doesn't bind any variable. To actually unifies
     * the terms use the other method unify.
     * @param term term to unify
     * @return true if the this object unify with the specified term.
     */
    public final boolean unifiable(final JIPTerm term)
    {
        return m_obj.unifiable(term.m_obj);
    }

    /** Clones this JIPTerm object.
     * @return Copy of this JIPTerm object .
     */
    public final Object clone()
    {
        return getJIPTerm(m_obj.copy(false));
    }

    /** Clears all variables in this JIPTerm object.
     */
    public final void clear()
    {
        m_obj.clear();
    }

    /** Unifies this JIPTerm object with the specified term.
     * If this JIPTerm object unifies with the specified term
     * this method unifies the terms and varTable will contains
     * the variables that has been bound.
     * @param term term to unify
     * @param varsTbl the hashtable containing the variable that has been bound.
     * @return true if the this object unify with the specified term.
     */
    public final boolean unify(final JIPTerm term, final Hashtable varsTbl)
    {
        Hashtable varTable1 = new Hashtable(10);
        if(m_obj.unify(term.m_obj, varTable1))
        {
            Variable var;
            Enumeration en = varTable1.elements();
            while(en.hasMoreElements())
            {
                var = ((Variable)en.nextElement());
                varsTbl.put(var, new JIPVariable(var));
            }

            return true;
        }
        else
        {
            return false;
        }
    }

    public JIPTerm getValue()
    {
    	return this;
    }

    /**
     * Returns a canonical representation of this JIPTerm object i.e. it ignores
     * operator declarations. It has the same behaviour as write_canonical/1
     * @return a canonical representation of this JIPTerm object
     */
    public String toString()
    {
        return m_obj.toString();
    }

    /**
     * Returns a string representation of this JIPTerm object using
     * the operator declarations contained in JIPEngine object.
     * It has the same behaviour as write/1
     * @param engine the JIPEngine object containing operator declarations
     * @return a string representation of this JIPTerm object
     */
    public String toString(final JIPEngine engine)
    {
        return m_obj.toString(engine);
    }

    /**
     * Returns a quoted string representation of this JIPTerm object using
     * the operator declarations contained in JIPEngine object.
     * It has the same behaviour as writeq/1
     * @param engine the JIPEngine object containing operator declarations
     * @return a string representation of this JIPTerm object
     */
    public String toStringq(final JIPEngine engine)
    {
        return m_obj.toStringq(engine);
    }

    /**
     * Gets an array of JIPVariable objects containing the variables in this JIPTerm object
     * @return an array of JIPVariable objects containing the variables in this JIPTerm object
     * @see com.ugos.jiprolog.engine.JIPVariable
     */
    public final JIPVariable[] getVariables()
    {
        JIPVariable[] vars;

        if(m_obj instanceof Atom || m_obj instanceof Expression || m_obj instanceof PString)
        {
            vars = new JIPVariable[0] ;
        }
        else if(m_obj instanceof Variable)
        {
            vars = new JIPVariable[1];
            vars[0] = new JIPVariable((Variable)m_obj);
        }
        else
        {
            m_varsVect = new Vector(5,2);
            final ConsCell cell = (ConsCell)m_obj;
            extractVariable(cell);

            vars = new JIPVariable[m_varsVect.size()];

            final Enumeration en = m_varsVect.elements();

            //int i = m_varsVect.size();
            int i = 0;
            while (en.hasMoreElements())
            {
                final Variable var = (Variable)en.nextElement();
                vars[i] = new JIPVariable(var);
                i++;
            }
        }

        return vars;
    }

    /**
     * Gets an hashtable of JIPVariable objects containing the variables in this JIPTerm object.<br>
     * Variable names are the keys to access to the hashtable.
     * @return an hashtable of JIPVariable objects containing the variables in this JIPTerm object.<br>
     * @see com.ugos.jiprolog.engine.JIPVariable
     */
    public final Hashtable getVariablesTable()
    {
        final JIPVariable[] vars = getVariables();
        final Hashtable varsTbl = new Hashtable(vars.length * 2);
        for(int i = 0; i < vars.length; i++)
        {
            varsTbl.put(vars[i].getName(), vars[i]);
        }

        return varsTbl;
    }

    JIPTerm(final PrologObject obj)
    {
        m_obj = obj;
    }

    private final void extractVariable(final PrologObject term)
    {
        if(term instanceof ConsCell)
        {
            extractVariable(((ConsCell)term).getHead());
            extractVariable(((ConsCell)term).getTail());
        }
        else if(term instanceof Variable)
        {
            //final String strVar = ((Variable)term).getName();
//            if (!((Variable)term).isShadow())
//            {
                if(!m_varsVect.contains(term))
                    m_varsVect.addElement(term);
//            }
        }
    }

    final PrologObject getRealTerm()
    {
        if (m_obj instanceof Variable)
        {
            if(((Variable)m_obj).isBounded())
                return ((Variable)m_obj).getObject();
        }

        return m_obj;
    }

    final PrologObject getTerm()
    {
        return m_obj;
    }

    @Override
    public boolean equals(Object obj)
    {
    	if(obj instanceof JIPTerm)
    		return this.unifiable((JIPTerm)obj);

    	return false;
    }

    /**
     * Gets the line number where the term is in the consulted file or stream
     * @return the line where this term is
     */
    public int getLine() {
		return m_obj.getLine();
	}

    /**
     * Gets the column where the term is in the line in the consulted file or stream
     * @return the column where this term is
     */
	public int getColumn() {
		return m_obj.getColumn();
	}

    /**
     * Gets the position from the beginning of the file where the term is in the consulted file or stream
     * @return the position where this term is
     */
	public int getPosition() {
		return m_obj.getPosition();
	}

    static final JIPTerm getJIPTerm(final PrologObject obj)
    {
        if(obj instanceof PString)
            return new JIPString((PString)obj);
        else if(obj instanceof List)
            return new JIPList((List)obj);
        else if(obj instanceof Atom)
            return new JIPAtom((Atom)obj);
        else if(obj instanceof Expression)
            return new JIPNumber((Expression)obj);
        else if(obj instanceof Functor)
            return new JIPFunctor((Functor)obj);
        else if(obj instanceof Variable)
            return new JIPVariable((Variable)obj);
        if(obj instanceof ConsCell)
            return new JIPCons((ConsCell)obj);
        if(obj instanceof Clause)
            return new JIPClause((Clause)obj);

        throw JIPRuntimeException.createRuntimeException(25, obj != null ? obj.toString() : "null");
    }

	@Override
	public int hashCode() {
		return getTerm().hashCode();
	}

}
