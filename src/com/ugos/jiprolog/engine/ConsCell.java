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

//import java.io.Serializable;
import java.util.Hashtable;

class ConsCell extends PrologObject //implements Serializable
{
    final static long serialVersionUID = 300000003L;

    static final ConsCell NIL = new ConsCell(null, null);

    protected PrologObject m_head;
    protected PrologObject m_tail;

    public ConsCell(final ConsCell master)
    {
        this((master == null) ? null : master.getHead(), (master == null) ? null : master.getTail());
    }

    public ConsCell(final PrologObject head, final PrologObject tail)
    {
        m_head = head;
        m_tail = tail;
    }

    public PrologObject copy(final boolean flat, final Hashtable<Variable, PrologObject> varTable)
    {
        if(m_head != null)
        {
            return new ConsCell(m_head.copy(flat, varTable),
                               (m_tail == null) ? null : m_tail.copy(flat, varTable));
        }
        else
        {
            return NIL;
        }
    }

    public boolean _unify(PrologObject obj, final Hashtable<Variable, Variable> table)
    {
//      System.out.println("*** ConsCell unify: " + this + " - " + obj);
//      System.out.println("*** obj " + obj.getClass());

        // if List or Clause or functor then false
        if((obj instanceof Functor) ||
           (obj instanceof List))
            return false;

        if(obj instanceof ConsCell)
        {
            if(m_head != null)
            {
                if(m_tail == null || m_tail.termEquals(ConsCell.NIL))
                {
                    if(((ConsCell)obj).m_tail == null || ((ConsCell)obj).m_tail.termEquals(ConsCell.NIL))
                    {
//                      System.out.println("*** 1 ");
//                      System.out.println("*** " + m_head.getClass());
//                      System.out.println("*** " + ((ConsCell)obj).m_head.getClass());
                        return m_head._unify(((ConsCell)obj).m_head, table);
                    }
                    else
                    {
//                      System.out.println("*** 2 ");
                        return m_head._unify(obj, table);
                    }
                }
                else //m_tail != null
                {
                    if(((ConsCell)obj).m_tail == null || ((ConsCell)obj).m_tail.termEquals(ConsCell.NIL))
                    {
//                      System.out.println("*** 3 ");
                        return ((ConsCell)obj).m_head._unify(this, table);
                    }
                    else
                    {
//                      System.out.println("*** 4 ");
                        return m_head._unify(((ConsCell)obj).m_head, table) &&
                               m_tail._unify(((ConsCell)obj).m_tail, table);
                    }
                }
            }
            else // m_head == null
            {
                if(m_tail != ((ConsCell)obj).m_tail)
                    return false;
                else if(((ConsCell)obj).m_head instanceof Variable)
                    return (((ConsCell)obj).m_head._unify(null, table));
                else
                    return (((ConsCell)obj).m_head == null);
            }
        }
        else if(obj instanceof Variable)
        {
            return obj._unify(this, table);
        }
        else if ((obj == null))
        {
            return (m_head == null);
        }

        //System.out.println("FALSE");
        return false;
    }

    public final PrologObject getHead()
    {
        return m_head;
    }

    public final PrologObject getTail()
    {
        return m_tail;
    }

    public final void setHead(final PrologObject head)
    {
        m_head = head;
    }

    public final void setTail(final PrologObject tail)
    {
        m_tail = tail;
    }

    public final void setLast(final PrologObject tail)
    {
        last(this).setTail(tail);
    }

    public ConsCell reverse() throws IllegalArgumentException
    {
        ConsCell     cell = null;

        PrologObject head = m_head;
        PrologObject tail = m_tail;

        while (head != null)
        {
            cell = new ConsCell(head, cell);

            if(tail instanceof Variable)
            {
                if(!((Variable)tail).isBounded())
                {
                    throw new IllegalArgumentException(this.toString());
                }
                tail = ((Variable)tail).getObject();

            }

            if (tail == null)
            {
                head = null;
            }
            else
            {
                head = ((ConsCell)tail).getHead();
                tail = ((ConsCell)tail).getTail();
            }
        }

        return cell;
    }

    public final boolean isNil()
    {
        return (m_head == null);
    }

    public final void clear()
    {
        if( m_head != null)
        {
            m_head.clear();

            if(m_tail != null)
                m_tail.clear();
        }
    }

    public final int getHeight()
    {
        return getHeight(this, 0);
    }

    public static final ConsCell append(final ConsCell list1, final ConsCell list2)
    {
        final ConsCell lastCons = last(list1);
        lastCons.setTail(list2);

        return list1;
    }

    static final ConsCell last(final ConsCell cell)
    {
        PrologObject tail = cell.getTail();
        if(tail == null || tail == ConsCell.NIL || tail == List.NIL)
        {
            return cell;
        }
        else
        {
            return last((ConsCell)BuiltIn.getRealTerm(tail));
        }
    }

    static final boolean isPartial(final ConsCell cell)
    {
    	PrologObject tail = cell.getTail();
        if(tail == null || tail == ConsCell.NIL || tail == List.NIL)
        {
            return false;
        }
        else if(tail instanceof Variable)
        {
        	tail = ((Variable)tail).getObject();
        	if(tail == null) // unbounded
        		return true;
        }

    	if(tail instanceof ConsCell)
    		return isPartial((ConsCell)tail);
    	else
			return false;
    }

    static final boolean isClosedOrPartial(final ConsCell cell)
    {
        PrologObject tail = cell.getTail();
        if(tail == null || tail == ConsCell.NIL || tail == List.NIL)
        {
            return true; // closed
        }
        else if(tail instanceof Variable)
        {
        	tail = ((Variable)tail).getObject();
        	if(tail == null) // unbounded
        		return true;  // partial
        }

    	if(tail instanceof ConsCell)
    		return isClosedOrPartial((ConsCell)tail);
    	else
			return false;
    }

    static final int getHeight(final ConsCell cell, final int nHeight)
    {
        if(cell == null || cell == ConsCell.NIL || cell == List.NIL)
        {
            return nHeight;
        }
        else
        {
        	try
        	{
        		return getHeight((ConsCell)BuiltIn.getRealTerm(cell.getTail()), nHeight + 1);
        	}
        	catch(ClassCastException ex)
        	{
        		return nHeight + 1;
        	}
        }
    }

    public boolean isPartial()
    {
    	return isPartial(this);
    }

    public boolean isClosedOrPartial()
    {
    	return isClosedOrPartial(this);
    }

    @Override
    public boolean termEquals(PrologObject obj)
    {
        if(obj instanceof ConsCell)
        {
        	int h1 = getHeight();
        	int h2 = ((ConsCell)obj).getHeight();
        	if(h1 != h2)
        	{
        		return false;
        	}
        	else //if(h1 == h2)
        	{
	            if(m_head == ((ConsCell)obj).m_head || m_head.termEquals(((ConsCell)obj).m_head))
	            {
	            	return (m_tail == ((ConsCell)obj).m_tail) ||
	            		   (m_tail == null && (((ConsCell)obj).m_tail.termEquals(List.NIL) || ((ConsCell)obj).m_tail.termEquals(ConsCell.NIL))) ||
	            		   (m_tail != null && m_tail.termEquals(((ConsCell)obj).m_tail));
	            }
        	}
        }
        else if(obj instanceof Variable)
        {
            if(((Variable)obj).isBounded())
                return termEquals(((Variable)obj).getObject());
        }
        else if(obj == null && this.termEquals(ConsCell.NIL))
        	return true;


        return false;
    }

    protected boolean lessThen(final PrologObject obj)
    {
        if(obj instanceof ConsCell)
        {
        	int h1 = getHeight();
        	int h2 = ((ConsCell)obj).getHeight();
        	if(h1 < h2)
        	{
        		return true;
        	}
        	else if(h1 == h2)
        	{
	            if(m_head == null)
	                return false;

	            if(m_head.lessThen(((ConsCell)obj).m_head))
	            {
	            	return true;
	            }
	            else if (m_head.termEquals(((ConsCell)obj).m_head))
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
        else if(obj instanceof Atom && this.unifiable(List.NIL))
		{
        	return true;
		}
        else if(obj instanceof Variable)
            if(((Variable)obj).isBounded())
                return lessThen(((Variable)obj).getObject());

        return false;
    }

    public final PrologObject getTerm(final int n)
    {
        PrologObject params = this;
        PrologObject param = null;

        int i = 0;

        while(i < n && params instanceof ConsCell)
        {
            param = ((ConsCell)params).getHead();
            params = BuiltIn.getRealTerm(((ConsCell)params).getTail());

            i++;
        }

        if(i == n)
            return param;
        else
            throw new IndexOutOfBoundsException(Integer.toString(n));
            //throw new JIPRuntimeException("ParameterIndexOutOfBounds" + Integer.toString(n));
    }

}
