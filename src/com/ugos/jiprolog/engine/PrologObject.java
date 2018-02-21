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
import java.util.Vector;
//#ifndef _MIDP
import java.io.Serializable;

import com.ugos.jiprolog.engine.WAM.Node;
//#endif

abstract class PrologObject implements Clearable, Serializable
{
    final static long serialVersionUID = 300000001L;

    private int line;
    private int column;
    private int position;

    public void setPosition(int line, int column, int position)
    {
    	this.line = line;
    	this.column = column;
    	this.position = position;
    }


    public int getLine() {
		return line;
	}


	public int getColumn() {
		return column;
	}


	public int getPosition() {
		return position;
	}


	public final boolean unifiable(final PrologObject obj)
    {
        Hashtable<Variable, Variable> vartbl = new Hashtable<Variable, Variable>(10);
        boolean bUnify = _unify(obj, vartbl);

        Enumeration<Variable> en = vartbl.keys();
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
            en = _varTbl.keys();
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
            en = _varTbl.keys();
            while(en.hasMoreElements())
                ((Clearable)en.nextElement()).clear();

            return false;
        }
    }

    public final String toString()
    {
        return PrettyPrinter.printTerm(this, null, true);
    }

    public final String toString(final JIPEngine engine)
    {
        return PrettyPrinter.printTerm(this, engine.getOperatorManager(),false);
    }

    public final String toStringq(final JIPEngine engine)
    {
        return PrettyPrinter.printTerm(this, engine.getOperatorManager(),true);
    }

    final String toString(final OperatorManager opMan)
    {
        return PrettyPrinter.printTerm(this, opMan, false);
    }

    public final PrologObject copy(boolean flat)
    {
        return copy(flat, new Hashtable(10));
    }

    public final PrologObject getRealTerm()
    {
        if(this instanceof Variable)
            return ((Variable)this).getObject();

        return this;
    }

    private Vector       m_varsVect;

    public final Variable[] getVariables()
    {
        Variable[] vars;

        if(this instanceof Atom || this instanceof Expression || this instanceof PString)
        {
            vars = new Variable[0] ;
        }
        else if(this instanceof Variable)
        {
            vars = new Variable[1];
            vars[0] = (Variable)this;
        }
        else
        {
        	m_varsVect = new Vector(5,2);
            final ConsCell cell = (ConsCell)this;
            extractVariable(cell);

            vars = new Variable[m_varsVect.size()];

            final Enumeration en = m_varsVect.elements();

            //int i = m_varsVect.size();
            int i = 0;
            while (en.hasMoreElements())
            {
                final Variable var = (Variable)en.nextElement();
                vars[i] = (Variable)var;
                i++;
            }
        }

        return vars;
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
    
    public abstract void clear();
    public abstract PrologObject copy(boolean flat, Hashtable<Variable, PrologObject> varTable);
    protected abstract boolean lessThen(PrologObject obj);
    protected abstract boolean _unify(PrologObject obj, Hashtable<Variable, Variable> varTbl);
    public abstract boolean termEquals(PrologObject obj);

    public abstract Enumeration<PrologRule> getRulesEnumeration(Node curNode, WAM wam);
}

























