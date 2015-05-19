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

import java.util.*;

class Multifile1 extends BuiltIn
{
    //protected ConsCell m_pred;
//    protected Vector m_predDefVect = new Vector();

//    public void bound()
//    {
//        final GlobalDB gdb = getJIPEngine().getGlobalDB();
//        for(int i = 0; i < m_predDefVect.size(); i++)
//        {
//            gdb.multifile((String)m_predDefVect.elementAt(i));
//        }
////        while (m_pred != null)
////        {
////            gdb.multifile(((Atom)m_pred.getHead()).getName());
////
////            m_pred = (ConsCell)m_pred.getTail();
////        }
//    }

    public boolean unify(final Hashtable varsTbl)
    {
        final Vector predDefVect = getPredDefVect();

        final GlobalDB gdb = getJIPEngine().getGlobalDB();
        for(int i = 0; i < predDefVect.size(); i++)
        {
            gdb.multifile((String)predDefVect.elementAt(i));
        }

        return true;
    }

    protected Vector getPredDefVect()
    {
        PrologObject pred = getRealTerm(getParam(1));

        if(pred instanceof Functor)
        {
            pred = new ConsCell(pred, null);
        }

        final Vector predDefVect = new Vector();

        while (pred != null)
        {

            try
            {
                String strPredDef;
                PrologObject head = getRealTerm(((ConsCell)pred).getHead());

                // head deve essere instanza di funtore /2 del tipo name/arity
                if(head instanceof Functor)
            	{
                	if (((Functor)head).getAtom().equals(Atom.SLASHSLASH))
	                {
	                    ConsCell params = ((Functor )head).getParams();
	                    strPredDef = new StringBuilder(((Atom)params.getHead()).getName()).append("/").append(((ConsCell)params.getTail()).getHead().toString(getJIPEngine())).toString();
	                    predDefVect.addElement(strPredDef);
	                }
	                else
	                {
	                    strPredDef = ((Functor)head).getName();
	                    predDefVect.addElement(strPredDef);
	                }
            	}
                else
                {
                    throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, pred);
                }

                pred = getRealTerm(((ConsCell)pred).getTail());
            }
            catch(ClassCastException ex)
            {
                throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, pred);
            }
        }

        return predDefVect;
    }
}
