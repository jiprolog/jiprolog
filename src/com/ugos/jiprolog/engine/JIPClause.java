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
 * JIPClause wraps a prolog clause
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPTerm
 */
public class JIPClause extends JIPTerm
{
    private final static long serialVersionUID = 300000001L;

    private JIPClause(final Functor func, final ConsCell body)
    {
        this(Clause.getClause(new Functor(":-/2", new ConsCell(func, new ConsCell(body, null)))));
    }

    private JIPClause(final Functor func)
    {
        this(Clause.getClause(func));
    }

    JIPClause(final Clause clause)
    {
        super(clause);
    }

    /** Creates a new JIPClause object
     * @param head the head of the clause
     * @param body the body of the clause. body can be null if the clause has no body.
     * @return new JIPClause object
     * @see com.ugos.jiprolog.engine.JIPFunctor
     * @see com.ugos.jiprolog.engine.JIPCons
      */
    public static final JIPClause create(final JIPFunctor head, final JIPCons body)
    {
       if(body == null)
            return new JIPClause((Functor)head.getTerm());
        else
            return new JIPClause((Functor)head.getTerm(), (ConsCell)body.getTerm());
    }

    /** Creates a new JIPClause object starting from e JIPTerm object
     * @param term the JIPTerm object to trasform in a clause
     * @return new JIPClause object
     * @throws com.ugos.jiprolog.engine.JIPParameterTypeException if term cannot be trasformed in a clause
     * @see com.ugos.jiprolog.engine.JIPTerm
     */
    public static final JIPClause create(final JIPTerm term)
    {
        return new JIPClause(Clause.getClause(term.getRealTerm()));
    }

    /** Returns the predicate in the head of this JIPClause object
      * @return head of this JIPClause object
      * @see com.ugos.jiprolog.engine.JIPFunctor
      */
    public final JIPFunctor getHead()
    {
        return new JIPFunctor((Functor)((ConsCell)getTerm()).getHead());
    }

    /** Returns the body of this JIPClause object
      * @return head of this JIPClause object
      * @see com.ugos.jiprolog.engine.JIPCons
      */
    public final JIPCons getBody()
    {
        ConsCell body = (ConsCell)((ConsCell)getTerm()).getTail();

        if(body != null)
            //return JIPTerm.getJIPTerm(body);
            return new JIPCons(body);
        else
            return null;
    }



}
