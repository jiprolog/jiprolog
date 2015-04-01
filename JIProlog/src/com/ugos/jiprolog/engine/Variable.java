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

final class Variable extends PrologObject//Serializable
{
    final static long serialVersionUID = 300000008L;

    private static final char ANONYMOUS = '^';
    private static final char SHADOW = '+';

    private String       m_strName;
    private PrologObject m_object;

    private int cyclic = -1;

    public Variable(final String strName)
    {
        m_strName = strName;
    }

    public Variable(final boolean bAnonymous)
    {
        if (bAnonymous)
            m_strName = ANONYMOUS + "" + hashCode();//m_nAddress;//m_nID.toString();
        else
            m_strName = SHADOW + "" + hashCode();//m_nAddress;//m_nID.toString();
    }

    public final PrologObject getObject()
    {
        PrologObject object = m_object;
        while(object instanceof Variable)
        {
//          System.out.println(object);
            object = ((Variable)object).m_object;
        }

        return object;
    }

    public final Variable lastVariable()
    {
        Variable var = this;
        PrologObject obj = m_object;
        while(obj instanceof Variable)
        {
            var = (Variable)obj;
            obj = ((Variable)obj).m_object;
        }

        return var;
    }

    public final String getName()
    {
        return m_strName;
    }

    public final int getAddress()
    {
        return lastVariable().hashCode();//m_nAddress;
    }

    public final PrologObject copy(final boolean flat, final Hashtable<Variable, PrologObject> varTable)
    {
        final Variable var = lastVariable();
        if(varTable.containsKey(var))
        {
            return varTable.get(var);
        }
        else
        {
            // bounded
            if(var.m_object != null)
            {
            	if(flat)
            	{
            		varTable.put(var, var);

            		PrologObject cobj = var.m_object.copy(flat, varTable);

            		varTable.put(var, cobj);


            		return cobj;
            	}
            	else
            	{
                    //System.out.println("copy " + m_strName);
                    final Variable newVar = new Variable(m_strName);
                    varTable.put(var, newVar);

            		newVar.m_object = var.m_object.copy(flat, varTable);

            		return newVar;
            	}
            }
            else
            {
                final Variable newVar = new Variable(m_strName);
                varTable.put(var, newVar);

        		return newVar;

            }


        }
    }

    public final boolean isBounded()
    {
        return getObject() != null;
    }

    public final boolean isAnonymous()
    {
        return m_strName.charAt(0) == ANONYMOUS;
    }

    public final boolean isShadow()
    {
        return m_strName.charAt(0) == SHADOW;
    }

    public final boolean _unify(final PrologObject obj, Hashtable<Variable, Variable> varTbl)
    {
//        System.out.println("*** Variable unify: " + this + " - " + obj);
        if(obj == this)
        {
            return true;
        }

        // per ottimizzazione
        final Variable var = lastVariable();

        if(obj == var)
        {
            return true;
        }
        else if(var.m_object != null)     // bounded
        {
            return var.m_object._unify(obj, varTbl);
        }
        else // unbounded
        {
            varTbl.put(var, var);

            if(obj == null)
            {
                var.m_object = List.NIL;
            }
            else
            {
                // Bound to obj
                if(obj instanceof Variable)
                {
                    Variable objVar = ((Variable)obj).lastVariable();

//                    System.out.println("*** lastVar: " + var + " - " + objVar);

                    if(objVar.m_object != null)  // obj bounded
                        var.m_object = objVar.m_object;
                    else
                    {
                        // entrambi unbounded
                        // controllo per evitare assegnazione ciclica
                        if(objVar != var)
                            var.m_object = obj;
                    }
                }
                else
                {
                    var.m_object = obj;
                }
            }

//            System.out.println("*** Unified to : " + this);
//            System.out.println("*** Unified to : " + var.m_object);
            return true;
         }
    }

    public final void clear()
    {
        m_object = null;
    }

    protected final boolean lessThen(final PrologObject obj)
    {
        if(isBounded())
            return getObject().lessThen(obj);
        else
            if(obj instanceof Variable)
                if(((Variable)obj).isBounded())
                    return true;
                else
                	return lastVariable().getName().compareTo(((Variable)obj).lastVariable().getName()) < 0;
                    //return m_nAddress < ((Variable)obj).m_nAddress;
//                    return getAddress() < ((Variable)obj).getAddress();

        return true;
    }

    public boolean cyclic()
    {
    	if(cyclic == -1)
    	{
    		cyclic = AcyclicTerm1.acyclic(this) ? 0 : 1;
    	}

    	return cyclic == 1;
    }

    @Override
    public boolean termEquals(PrologObject obj)
    {
    	if(isBounded())
    	{
    		return lastVariable().m_object.termEquals(obj);
    	}
    	else if(obj instanceof Variable)
        {
    		if (((Variable)obj).isBounded())
    			return false;
    		else
    			return lastVariable().getName().compareTo(((Variable)obj).lastVariable().getName()) == 0;//lastVariable().getAddress() == ((Variable)obj).lastVariable().getAddress();

        }

        return false;
    }
/*
    public void finalize() throws Throwable
    {
            System.out.println("Rilasciata variabile: " + m_nID);
            super.finalize();
    }
*/
}


