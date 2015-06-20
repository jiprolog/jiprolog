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

//import java.io.Serializable;
import java.util.Hashtable;

final class PString extends List //implements Serializable
{
    final static long serialVersionUID = 300000007L;

    private String m_strString;
    private boolean m_chars;
    //private int    m_nHashValue;

//    public PString(final PrologObject head, final ConsCell tail, JIPEngine engine)
//    {
//        this(new List(head, tail), engine);
//
//    }

//    // ottimizzazione
//    private PString(final PrologObject head, final ConsCell tail, String string)
//    {
//        super(new List(head, tail));
//        m_strString = string;
//    }

    public PString(final List string, boolean chars)
    {
        super(string);

        if(string.isPartial())
        	throw new JIPInstantiationException();

        m_chars = chars;

        PrologObject tail = string;
        PrologObject head = ((ConsCell)tail).getHead();

        m_strString = "";

        while (head != null)
        {
            if (head instanceof Variable)
            {
                if (((Variable)head).isBounded())
                {
                    head = ((Variable)head).getObject();
                }
                else
                {
                	throw new JIPInstantiationException();
                }
            }

            if (head instanceof Expression)
            {
            	if(chars)
            		throw new JIPRepresentationException("character_code");

                Expression ascii = (Expression)head;

                if (!ascii.isInteger())
                {
                    throw new JIPTypeException(JIPTypeException.INTEGER, head);
                }

                int nAscii = (int)ascii.getValue();

                if(nAscii < 0 || nAscii > 255)
                	throw new JIPRepresentationException("character_code");
//                	throw new JIPTypeException(JIPTypeException.INTEGER, head);

                m_strString += String.valueOf((char)nAscii);
            }
            else if (head instanceof Atom)
            {
            	if(!chars)
            		throw new JIPTypeException(JIPTypeException.CHARACTER, head);

            	String a = ((Atom)head).getName();

            	if(a.length() > 1)
            	{
            		throw new JIPTypeException(JIPTypeException.INTEGER, head);
            	}

            	char code = a.charAt(0);

                m_strString += String.valueOf(code);
            }
            else
            {
            	throw new JIPTypeException(JIPTypeException.CHARACTER, head);
            }

            if (tail instanceof Variable)
            {
                tail = ((Variable)tail).getObject();;
            }

            if(tail == null)
            {
            	throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, head);
            }
            else if (!(tail instanceof ConsCell))
            {
            	throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, head);
            }

//          System.out.println("1" + tail.getClass());

            tail = ((ConsCell)tail).getTail();

            if (tail instanceof Variable)
            {
                tail = ((Variable)tail).getObject();;
            }

            if (tail == null)
            {
                head = null;
            }
            else
            {
                head = ((ConsCell)tail).getHead();
            }
        }
    }

    public PString(final PString string)
    {
    	super(string);
    	m_chars = string.m_chars;
    	m_strString = string.m_strString;
    }

    public PString(final String strString, boolean chars)
    {
        super(getList(strString, chars));
        m_strString   = strString;
    }

    @Override
    public PrologObject copy(final boolean flat, final Hashtable<Variable, PrologObject> varTable)
    {
    	return new PString(this);
    }

    @Override
    public final boolean _unify(PrologObject obj, final Hashtable table)
    {
    	if(obj instanceof Variable)
        {
        	if(((Variable)obj).isBounded())
        		obj = ((Variable)obj).getObject();
        	else
        		return ((Variable)obj)._unify(this, table);
        }

        if (obj instanceof List)
        {
            return super._unify(obj, table);
        }
//        else if(obj instanceof Variable)
//        {
//            return ((Variable)obj)._unify(this, table);
//        }
        else
        {
            return false;
        }
    }

    private static final List getList(final String string, boolean chars)
    {
        List retList = null;

        for(int i = string.length() - 1; i >= 0; i--)
        {
            retList = new List(chars ? Atom.createAtom(string.substring(i, i+1)) : Expression.createNumber(string.charAt(i)), retList);
        }

        return retList;
    }

    public final String getString()
    {
        return m_strString;
    }

    protected final boolean lessThen(PrologObject obj)
    {
    	if(obj instanceof Variable)
        {
            if(((Variable)obj).isBounded())
                obj = ((Variable)obj).getObject();
            else
            	return false;
        }

        if(obj instanceof PString)
            return m_strString.compareTo( ((PString)obj).m_strString) < 0;
        if(obj instanceof List)
            return super.lessThen(obj);
        else if(obj instanceof Atom)
            return false;
        else if(obj instanceof Expression)
            return false;
//        else if(obj instanceof Variable)
//            if(((Variable)obj).isBounded())
//                return lessThen(((Variable)obj).getObject());
//            else
//                return false;

        return true;
    }

	@Override
	public int hashCode() {
		return m_strString.hashCode();
	}
}
