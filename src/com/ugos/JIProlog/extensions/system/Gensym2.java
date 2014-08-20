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

package com.ugos.JIProlog.extensions.system;

import com.ugos.JIProlog.engine.*;

import java.util.*;

public class Gensym2 extends JIPXCall
{
    private static int s_nCount = 0;
    
    public final boolean unify(final JIPCons input, Hashtable varsTbl)
    {
        JIPTerm base = input.getNth(1);
        
        // check if input is a variable
        if (base instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)base).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                base = ((JIPVariable)base).getValue();
            }
        }

        if (!(base instanceof JIPAtom))
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM);
        }
        
        JIPAtom unique = JIPAtom.create(((JIPAtom)base).getName() + s_nCount);
        
        return input.getNth(2).unify(unique, varsTbl);
    }
        
    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

