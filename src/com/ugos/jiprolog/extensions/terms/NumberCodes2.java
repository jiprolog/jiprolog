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

public class NumberCodes2 extends JIPXCall
{
    public final boolean unify(final JIPCons input, Hashtable<JIPVariable, JIPVariable> varsTbl)
    {
        JIPTerm number = input.getNth(1);
        JIPTerm codes  = input.getNth(2);

        // check if input is a variable
        if (number instanceof JIPVariable)
        {
            // try to extract the term
            if(((JIPVariable)number).isBounded())
            {
                //extracts the term
                number = ((JIPVariable)number).getValue();
            }
        }

        if (number instanceof JIPNumber)
		{
	            String strNumber;
	            if(((JIPNumber)number).isInteger())
	                strNumber = Integer.toString((int)((JIPNumber)number).getDoubleValue());
	            else
	                strNumber = Double.toString(((JIPNumber)number).getDoubleValue());

	            number = JIPString.create(strNumber, false);
	        }
        else if (number instanceof JIPVariable)
        {
        	// means number unbounded
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
                throw new JIPSyntaxErrorException("not_a_number");
            }
            else if (codes instanceof JIPList)
				try {
					String strVal = (JIPString.create((JIPList)codes)).getStringValue();

					// remove leading whitespace
					strVal = strVal.replaceAll("^\\s+", "");

					// trailing whitespace is considered a syntax error
					if(strVal.length() != strVal.replaceAll("\\s+$", "").length())
					    throw new JIPSyntaxErrorException("not_a_number");

//                	if(strVal.startsWith("0''") && strVal.length() > 3)
//                		codes = JIPNumber.create(strVal.codePointAt(3));
//                	else if(strVal.startsWith("0\'") && strVal.length() > 3)
//                		codes = JIPNumber.create(strVal.codePointAt(3));
//                	else
               		if(strVal.startsWith("0'"))
						codes = JIPNumber.create(strVal.codePointAt(2));
                	else if(strVal.startsWith("0x"))
                		codes = JIPNumber.create(Integer.parseInt(strVal.substring(2), 16));
                	else if(strVal.startsWith("0o"))
                		codes = JIPNumber.create(Integer.parseInt(strVal.substring(2), 8));
                	else if(strVal.startsWith("0b"))
                		codes = JIPNumber.create(Integer.parseInt(strVal.substring(2), 2));
                	else
                	{
	                	Double d = Double.parseDouble(strVal);
	                	if(strVal.contains("."))
	                		codes = JIPNumber.create(d);
	                	else
	                		codes = JIPNumber.create(d.intValue());
                	}
            	} catch (NumberFormatException e) {
	                throw new JIPSyntaxErrorException("not_a_number");
				}
            else
            {
                throw new JIPTypeException(JIPTypeException.LIST, codes);
            }
        }
        else
        {
            throw new JIPTypeException(JIPTypeException.NUMBER, number);
        }

        return number.unify(codes, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

