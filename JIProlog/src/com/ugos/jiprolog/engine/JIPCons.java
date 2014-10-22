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

/**
 * JIPList wraps a prolog cons (a cons is composed by a set of terms separated by comma eventually enclosed in parenthesis: a,b,c or (a,b,c,).
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPTerm
 */
public class JIPCons extends JIPTerm
{
    private final static long serialVersionUID = 300000001L;
    
    /** Empty cons or nil
     */
    public static final JIPCons NIL = new JIPCons(ConsCell.NIL);
    
    /** Creates a new cons object
     * @param head the head of the cons
     * @param tail the tail the cons
     * @return new JIPCons object
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public static final JIPCons create(final JIPTerm head, final JIPTerm tail)
    {
        if(head != null)
        {
            if(tail != null)
                return new JIPCons(new ConsCell(head.getTerm(), tail.getTerm()));
            else
                return new JIPCons(new ConsCell(head.getTerm(), null));
        }
        else
        {
            if(tail != null)
                return new JIPCons(new ConsCell(null, tail.getTerm()));
            else
                return new JIPCons(ConsCell.NIL);
        }
    }

    /** Returns a new JIPCons object by appending cons2 to cons1
     * @param cons1 first cons
     * @param cons2 cons to append at the end of the first one.
     * @return new JIPCons object by appending cons1 and cons2
     */
    public final static JIPCons append(final JIPCons cons1, final JIPCons cons2)
    {
        ConsCell cell = ConsCell.append((ConsCell)cons1.getTerm(), (ConsCell)cons2.getTerm());
        return new JIPCons(cell);
    }
    
    /** Returns a new JIPCons object by reversing this JIPCons object
      * @return new JIPCons object by reversing this JIPCons object
      */
    public final JIPCons reverse()
    {
        return new JIPCons(((ConsCell)getTerm()).reverse());
    }

    JIPCons(final ConsCell cons)
    {
        super(cons);
    }
    
    /** Returns the head of this JIPCons object
     * @return head of this JIPCons
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public final JIPTerm getHead()
    {
        final PrologObject head = ((ConsCell)getTerm()).getHead();
        
        if (head == null)
        {
            return null;
        }
        else
        {
            return JIPTerm.getJIPTerm(head);
        }
    }
    
    /** Returns the tail of this JIPCons object
     * @return tail of this JIPCons object
     * @see com.ugos.jiprolog.engine.JIPTerm
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
    
    /** Returns the nth term in this cons. <br>
     * Raises ArrayIndexOutOfBound if the parameter is out of bound
     * @param n index ot the term to extract
     * @return the nth term in this cons.
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public final JIPTerm getNth(int n)
    {
        return JIPTerm.getJIPTerm(((ConsCell)getTerm()).getTerm(n));
    }
    
    /** Returns true if this cons is nil. <br>
     */
    public final boolean isNIL()
    {
        return getHead() == null;
    }
    
    /** Returns the number of elements in this cons object.<br>
     * @return the number of elements in this cons object.
     */
    public final int getHeight()
    {
        return ((ConsCell)getTerm()).getHeight();
    }
}
