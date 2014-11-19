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

public class Name2 extends JIPXCall
{
    public final boolean unify(final JIPCons input, Hashtable<JIPVariable, JIPVariable> varsTbl)
    {
        JIPTerm atom   = input.getNth(1);
        JIPTerm string = input.getNth(2);

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

        if (atom instanceof JIPNumber)
        {
            String strAtom;
            if(((JIPNumber)atom).isInteger())
                strAtom = Integer.toString((int)((JIPNumber)atom).getDoubleValue());
            else
                strAtom = Double.toString(((JIPNumber)atom).getDoubleValue());

            atom = JIPString.create(strAtom);
        }
        else if (atom instanceof JIPAtom)
        {
            String strAtom = ((JIPAtom)atom).getName();
            if(strAtom.equals(""))
            {
                atom = JIPList.NIL;
            }
//            else if(strAtom.charAt(0) == 39)
//            {
//                strAtom = strAtom.substring(1, strAtom.length() - 1);
//                atom = JIPString.create(strAtom);
//            }
            else
            {
                atom = JIPString.create(strAtom);
            }
        }
        else if (atom instanceof JIPVariable)
        {
        	// means atom unbounded
            if (string instanceof JIPVariable)
            {
                if (((JIPVariable)string).isBounded())
                {
                    string = ((JIPVariable)string).getValue();
                }
                else
                {
                    throw new JIPParameterUnboundedException(2);
                }
            }

            if(string == JIPList.NIL)
            {
                string = JIPAtom.create("");
            }
            else if (string instanceof JIPList)
            {

                // check if number of atom
                String strVal = (JIPString.create((JIPList)string)).getStringValue();

                if(strVal.startsWith(" ") || strVal.endsWith(" "))
                {
                	string = JIPAtom.create(strVal);
                }
                else
                {
	                try
	                {
	                    double dbVal = Double.valueOf(strVal).doubleValue();
	                    string = JIPNumber.create(dbVal);
	                }
	                catch(NumberFormatException ex)
	                {
	                    string = JIPAtom.create(strVal);
	                }
                }
            }
            else
            {
                throw new JIPParameterTypeException(2,JIPParameterTypeException.LIST);
            }
        }
        else
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.UNDEFINED);
        }

        return atom.unify(string, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

