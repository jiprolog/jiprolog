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

/**
 * JIPFunctor wraps a prolog functor
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPTerm
 */
public class JIPFunctor extends JIPTerm
{
    private final static long serialVersionUID = 300000001L;
    
    /** Creates a new JIPFunctor object
     * @param strName the name of the functor
     * @param params the list of parameters
     * @return new JIPFunctor object
     * @see com.ugos.jiprolog.engine.JIPFunctor
     */
    public static final JIPFunctor create(final String strName, final JIPCons params)
    {
        if(params == null)
            return new JIPFunctor(new Functor(strName + "/0", null));
        else
        {
            final ConsCell cons = (ConsCell)params.getTerm();
            return new JIPFunctor(new Functor(strName + "/" + Integer.toString(cons.getHeight()), cons));
        }
    }
    
    JIPFunctor(Functor func)
    {
        super(func);
    }
    
    /** Returns the name of this JIPfunctor object
      * @return name of this JIPFunctor object
      */
    public final String getName()
    {
        return ((Functor)getTerm()).getFriendlyName();
    }
    
    /** Returns the Atom of this JIPfunctor object
     * @return atom of this JIPFunctor object
     */
   public final JIPTerm getTerm(int index)
   {
	   if(index == 0)
	   {
		   return new JIPAtom(((Functor)getTerm()).getAtom());
	   }
	   else
	   {
		   final ConsCell params = ((Functor)getTerm()).getParams();
		   return JIPTerm.getJIPTerm(params.getTerm(index));
	   }
   }
    /** Returns the definition name of this JIPfunctor object - <predname>/<arity>
      * @return definition name of this JIPFunctor object
      */
    public final String getDefinition()
    {
        return ((Functor)getTerm()).getName();
    }
    
    /** Returns the list of parameters of this JIPFunctor object
      * @return list of parameters of this JIPFunctor object or null if the functor doesn't have any parameter
      * @see com.ugos.jiprolog.engine.JIPCons
      */
    public final JIPCons getParams()
    {
        final ConsCell params = ((Functor)getTerm()).getParams();
        
        if(params != null)
            return new JIPCons(params);
        else
            return null;//JIPCons.NIL;
    }
    
    
    
    /** Returns the arity of this JIPFunctor object
     * @return arity of this JIPFunctor object
      */
    public final int getArity()
    {
        return ((Functor)getTerm()).getArity();
    }
}
