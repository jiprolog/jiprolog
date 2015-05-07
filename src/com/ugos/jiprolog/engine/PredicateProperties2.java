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

final class PredicateProperties2 extends BuiltIn
{
    //private Atom m_nextProp = null;
    public final boolean unify(final Hashtable varsTbl)
    {
        PrologObject parm = getRealTerm(getParam(1));
        if(parm == null)
            throw new JIPInstantiationException(1);
        else if((parm instanceof Atom))
        {
        	parm = new Functor((Atom)parm);
        }
        else if(!(parm instanceof Functor))
            throw new JIPTypeException(JIPTypeException.FUNCTOR, parm);

        Functor funct = (Functor)parm;
//      System.out.println("Name: " + funct.getName() + "|||");

//        if(!funct.getName().equals("//2"))
//        {
        	funct = new Functor(Atom.SLASHSLASH, new ConsCell(Atom.createAtom(funct.getFriendlyName()), new ConsCell(Expression.createNumber(funct.getArity()), null)));
//        }
//
//            throw new JIPTypeException(JIPTypeException.PREDICATE_INDICATOR, parm);

        if(funct.getParams().getHead() == null)
        	return false;

        Clause clause = Clause.getClause(funct.getParams().getHead(), false);
        String strFunc = clause.getHead().toString(getJIPEngine()) + "/" + ((ConsCell)funct.getParams().getTail()).getHead().toString(getJIPEngine());

        List propsList = null;

        if(getJIPEngine().getGlobalDB().isDynamic(strFunc))
            propsList = new List(Atom.createAtom("dynamic"), propsList);
        else
            propsList = new List(Atom.createAtom("static"), propsList);


//      System.out.println(propsList);
        if(getJIPEngine().getGlobalDB().isMultifile(strFunc))
            propsList = new List(Atom.createAtom("multifile"), propsList);

//      System.out.println(propsList);
        if(getJIPEngine().getGlobalDB().isModuleTransparent(strFunc))
            propsList = new List(Atom.createAtom("transparent"), propsList);

//      System.out.println(propsList);
        if(getJIPEngine().getGlobalDB().isSystem(strFunc))
        {
            propsList = new List(Atom.createAtom("built_in"), propsList);
            propsList = new List(Atom.createAtom("visible"), propsList);
        }

        if(getJIPEngine().getGlobalDB().isExternal(strFunc))
        {
            propsList = new List(Atom.createAtom("built_in"), propsList);
            propsList = new List(Atom.createAtom("foreign"), propsList);
            propsList = new List(Atom.createAtom("visible"), propsList);

        }

        //      System.out.println(propsList);
        String strFile = getJIPEngine().getGlobalDB().getFile(strFunc);
        if(strFile != null)
        {
            // se è definito in un file
            propsList = new List(new Functor("file/1", new ConsCell(Atom.createAtom(strFile), null)), propsList);

            // se è interpretato
            if(!BuiltInFactory.isBuiltIn(strFunc) && !getJIPEngine().getGlobalDB().isExternal(strFunc))
            {
                propsList = new List(Atom.createAtom("interpreted"), propsList);
            }
        }

        if(propsList == null)
            return false;

        return getParam(2).unify(propsList, varsTbl);
    }

    public final boolean hasMoreChoicePoints()
    {
        return false;
    }
}
