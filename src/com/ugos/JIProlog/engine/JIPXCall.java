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

package com.ugos.JIProlog.engine;
import java.util.*;

/**
 * JIPXCall is the base class for custom built-in predicates.<br>
 * See the section <i>"How to write a custom built-in predicate"</i> in the Reference Manual
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 */
public abstract class JIPXCall
{
    private BuiltIn   m_builtIn;
    
    /** Called by xcall/2 to initialize this JIPXCall object
     */
    protected void init()
    {
        
    }
    
    final void init(final BuiltIn builtIn)
    {
        m_builtIn   = builtIn;
        init();
    }

   /** Returns the current running JIPEngine object
     * @return current running JIPEngine object
     * @see com.ugos.JIProlog.engine.JIPEngine
     */
    public final JIPEngine getJIPEngine()
    {
        return m_builtIn.getJIPEngine();
    }
        
   /** Returns the handle of the current running query
     * @return handle of the current running query
     */
    public final int getQueryHandle()
    {
        return m_builtIn.getQueryHandle();
    }
    
   /** Returns the functor of the predicate invoking this instance of JIPXCall class
     * @return functor of the predicate invoking this instance of JIPXCall class
     * @see com.ugos.JIProlog.engine.JIPFunctor
     */
    public final JIPFunctor getPredicate()
    {
        return new JIPFunctor(m_builtIn.getPredicate());
    }
    
    /** Invoked by xcall/2 to check if the custom predicate unify with the passed parameters
     * For more information see the section <i>"How to write a custom built-in predicate"</i> in the Reference Manual.<br>
     * @param params the parameters passed to the predicate.
     * @return true if the custom predicates unify with the parameter passed, false otherwise
     * @see com.ugos.JIProlog.engine.JIPCons
     */
    public abstract boolean unify(JIPCons params, Hashtable<JIPVariable, JIPVariable> varsTbl);
    
    /** Invoked by xcall/2 to bind the custom predicate to the passed parameters.
     * For more information see the section <i>"How to write a custom built-in predicate"</i> in the Reference Manual.<br>
     * @return JIPTerm object resulting from the computation of this JIPXCall object
     * @see com.ugos.JIProlog.engine.JIPCons
     */
//    public abstract void bind(JIPCons params);
        
    /** Invoked by xcall/2 to check if the custom predicate has more choice points on backtracking.<br>
     * @return true if the custom predicate has more choice points, false otherwise.
     */
    public abstract boolean hasMoreChoicePoints();
}

