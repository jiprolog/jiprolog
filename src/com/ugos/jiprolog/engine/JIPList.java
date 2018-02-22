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

import java.util.ArrayList;
import java.util.Vector;

/**
 * JIPList wraps a prolog list
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPTerm
 */
public class JIPList extends JIPTerm
{
    private final static long serialVersionUID = 300000001L;

    JIPList(List list)
    {
        super(list);
    }

    /** Empty list or nil []
     */
    public static final JIPList NIL = new JIPList(List.NIL);

    /** Creates a new List
     * @param head the head of the List
     * @param tail the tail the List
     * @return new JIPList object
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public static final JIPList create(final JIPTerm head, final JIPTerm tail)
    {
        if(head != null)
        {
            if(tail != null)
                return new JIPList(new List(head.getTerm(), tail.getTerm()));
            else
                return new JIPList(new List(head.getTerm(), null));
        }
        else
        {
            if(tail != null)
                return new JIPList(new List(null, tail.getTerm()));
            else
                return new JIPList(List.NIL);
        }
    }

    /** Creates a new List
     * @param termList vector of JIPTerm to transform in a prolog list
     * @return new JIPList object
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public static final JIPList create(final Vector<JIPTerm> termList)
    {
    	JIPList list = null;
    	
    	for(JIPTerm term : termList)
    	{
    		list = JIPList.create(term, list);
    	}
    	
    	return list != null ? list.reverse() : NIL;
    }
    
    /** Creates a new List
     * @param termList list of JIPTerm to transform in a prolog list
     * @return new JIPList object
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public static final JIPList create(final ArrayList<JIPTerm> termList)
    {
    	JIPList list = null;
    	
    	for(JIPTerm term : termList)
    	{
    		list = JIPList.create(term, list);
    	}
    	
    	return list.reverse();
    }
    
    /** Returns a new JIPList object by appending list2 to list1
     * @param head the list1 of the first list
     * @param tail the list2 the second list
     * @return new JIPList object by appending list1 and list2
     */
    public static final JIPList append(final JIPList head, final JIPList tail)
    {
        List cell = new List(ConsCell.append((ConsCell)head.getTerm(), (ConsCell)tail.getTerm()));
        return new JIPList(cell);
    }

    /** Returns a new JIPList object by reversing this JIPList object
      * @return new JIPList object by reversing this JIPList object
      */
    public final JIPList reverse()
    {
        return new JIPList(new List(((ConsCell)getTerm()).reverse()));
    }

    /** Returns the length of this JIPList object
     * @return the length of this JIPList object
     */
   public final int length()
   {
       return ((ConsCell)getTerm()).getHeight();
   }

    /** Returns the head of this JIPList object
     * @return head of this JIPList
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public final JIPTerm getHead()
    {
        PrologObject head = ((ConsCell)getTerm()).getHead();

        if (head == null)
        {
            return null;
        }
        else
        {
            return JIPTerm.getJIPTerm(head);
        }
    }

    /** Returns the tail of this JIPList object
     * @return tail of this JIPList object
     */
    public final JIPTerm getTail()
    {
        final PrologObject tail = ((ConsCell)getTerm()).getTail();

        if (tail == null)
        {
            return null;
        }
        else
        {
            return JIPTerm.getJIPTerm(tail);
        }
    }

    /** Returns the nth term in this list. <br>
     * Raises ArrayIndexOutOfBound if the parameter is out of bound
     * @param n index ot the term to extract
     * @return the nth term in this list.
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public final JIPTerm getNth(final int n)
    {
        return JIPTerm.getJIPTerm(((ConsCell)getTerm()).getTerm(n));
    }

    /** Returns true if this list is []. <br>
     */
    public final boolean isNIL()
    {
        return ((List)getTerm()).isNil();
    }

    /** Returns true if this cons object is a partial cons list.<br>
     * @return true if this cons object is a partial cons list..
     */
    public boolean isPartial()
    {
    	return  ((ConsCell)getTerm()).isPartial();
    }

    /** Returns true if this cons object is a closed or a partial cons list.<br>
     * @return true if this cons object is a closed or a partial cons list..
     */
    public boolean isClosedOrPartial()
    {
    	return  ((ConsCell)getTerm()).isClosedOrPartial();
    }

    /** Returns the index in the list where the given term is found<br>
     * It works like member/2
     * @return the index in the list where the given term is found<br>
     */
    public int member(JIPTerm term)
    {
    	return ((List)getTerm()).member(term.getTerm());
    }

    /** Returns the terms in the list as Java List<br>
     * @return the terms in the list as Java List<br>
     */
    public java.util.List<JIPTerm> getTerms()
    {
    	ArrayList<JIPTerm> termList = new ArrayList<JIPTerm>();
    	
    	JIPTerm head = getHead();
    	JIPTerm tail = getTail();
    	
    	while(head != null && head != NIL)
    	{
    		termList.add(head);
    		head = tail;
    		if(tail instanceof JIPList)
    		{
    			tail = ((JIPList)tail).getTail();
    		}
    		else
    		{
    			tail = NIL;
    		}
    	}
    	
    	return termList;
    }
    

}
