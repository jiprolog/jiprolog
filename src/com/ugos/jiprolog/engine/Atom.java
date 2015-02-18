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

final class Atom extends PrologObject //implements Serializable
{
    final static long serialVersionUID = 300000001L;

    static final Hashtable<String, Atom> s_atomTable = new Hashtable<String, Atom>(100);

    final static Atom FSEMICOLON = Atom.createAtom(";/2");
    final static Atom FIF        = Atom.createAtom("->/2");
    final static Atom FSTARIF    = Atom.createAtom("*->/2");
    final static Atom FCOLON     = Atom.createAtom(":/2");

    private String m_strAtom;
    private int m_nHashValue;

    public static final Atom createAtom(final String strAtom)
    {
        if (s_atomTable.containsKey(strAtom))
        {
//          System.out.println("***** found ******");
            return s_atomTable.get(strAtom);
        }

//      System.out.println("***** not found ******");
        Atom atom = new Atom(strAtom);
        s_atomTable.put(strAtom, atom);

        return atom;
    }

    public static final int atoms()
    {
        return s_atomTable.size();
    }
/*
    public static void printAtoms()
    {
        Enumeration enum = s_atomTable.elements();
        while (enum.hasMoreElements())
        {
            System.out.println(enum.nextElement());
        }
    }
*/
    private Atom(final String strAtom)
    {
        m_strAtom   = strAtom;
        m_nHashValue = m_strAtom.hashCode();
    }

    public final PrologObject copy(final boolean flat, final Hashtable<Variable, PrologObject> varTable)
    {
        return this;
    }

    public final boolean _unify(final PrologObject obj, final Hashtable<Variable, Variable> table)
    {
        if (obj instanceof Atom)
        {
//            System.out.println("*** Atom unify: " + this.m_strAtom + " - " + ((Atom)obj).m_strAtom);
//            System.out.println("*** m_nHashValue: " + m_nHashValue+ " - obj.hashval(): " + ((Atom)obj).m_nHashValue);

            return m_nHashValue == ((Atom)obj).m_nHashValue;
        }
        else if(obj instanceof Variable)
        {
//            System.out.println("*** Atom unify Var: " + this + " - " + obj);
//            System.out.println("*** m_nHashValue: " + m_nHashValue+ " - obj.hashval(): " + ((Atom)obj).m_nHashValue);

            return ((Variable)obj)._unify(this, table);
        }
        else
        {
//            System.out.println("*** Atom not unify : " + this);
//            if(obj != null)
//                System.out.println("*** obj.getClass(): " + obj.getClass());

            return false;
        }
    }

    public final void clear()
    {
        // Do nothing
    }

    public final String getName()
    {
        return m_strAtom;
    }

//    variable precedes floating point precedes integer precedes atom precedes compound.
    protected final boolean lessThen(final PrologObject obj)
    {
        if(obj instanceof Atom)
            return m_strAtom.compareTo( ((Atom)obj).m_strAtom) < 0;
        else if(obj instanceof Variable && ((Variable)obj).isBounded())
            return lessThen(((Variable)obj).getObject());
        else if(obj.unifiable(List.NIL))
        {
        	return false;
        }
        else if(obj instanceof ConsCell)
        	return true;

        return false;
    }

    @Override
    public boolean termEquals(PrologObject obj)
    {
        if(obj instanceof Atom)
            return m_strAtom.equals(((Atom)obj).m_strAtom);
        else if(obj instanceof Variable && ((Variable)obj).isBounded())
            return termEquals(((Variable)obj).getObject());

        return false;
    }
}
