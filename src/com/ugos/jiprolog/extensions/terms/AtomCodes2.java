/*****************************************
 * 27/03/2003
 *
 * Copyright (C) 1999-2003 Ugo Chirico
 * http://www.ugochirico.com
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
 *****************************************/

package com.ugos.jiprolog.extensions.terms;

import com.ugos.jiprolog.engine.*;

import java.util.*;

public class AtomCodes2 extends JIPXCall
{
    public final boolean unify(final JIPCons input, Hashtable<JIPVariable, JIPVariable> varsTbl)
    {
        JIPTerm atom   = input.getNth(1);
        JIPTerm codes = input.getNth(2);

        // check if input is a variable
        if (atom instanceof JIPVariable)
        {
            // try to extract the term
            if(((JIPVariable)atom).isBounded())
            {
                //extracts the term
                atom = ((JIPVariable)atom).getValue();
            }
        }

        if (atom instanceof JIPAtom)
        {
            String strAtom = ((JIPAtom)atom).getName();
            if(strAtom.equals(""))
            {
                atom = JIPList.NIL;
            }
            else
            {
                atom = JIPString.create(strAtom, false, getJIPEngine());
            }
        }
        else if (atom instanceof JIPVariable)
        {
        	// means atom unbounded
            if (codes instanceof JIPVariable)
            {
                if (((JIPVariable)codes).isBounded())
                {
                    codes = ((JIPVariable)codes).getValue();
                }
                else
                {
                    throw new JIPParameterUnboundedException(2);
                }
            }

            if(codes == JIPList.NIL)
            {
                codes = JIPAtom.create("");
            }
            else if (codes instanceof JIPList)
            {
                String strVal = (JIPString.create((JIPList)codes, getJIPEngine())).getStringValue();

                if(strVal.startsWith(" ") || strVal.endsWith(" "))
                {
                	codes = JIPAtom.create(strVal);
                }
                else
                {
                	codes = JIPAtom.create(strVal);
                }
            }
            else
            {
                throw new JIPTypeException(JIPTypeException.LIST, codes);
            }
        }
        else if(atom.unifiable(JIPList.NIL))// ||
//        		(atom instanceof JIPCons && ((JIPCons)atom).getHead().unifiable(JIPCons.NIL) && ((JIPCons)atom).getTail().unifiable(JIPCons.NIL)))
        {
        	atom =  JIPString.create("[]", false, getJIPEngine());
        }
        else
        {
            throw new JIPTypeException(JIPTypeException.ATOM, atom);
        }

        return atom.unify(codes, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

