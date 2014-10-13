/*
 *
 * Copyright (C) 1999-2004 Ugo Chirico
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


package com.ugos.JIProlog.extensions.io;

import com.ugos.JIProlog.engine.*;

import java.io.*;
import java.util.*;

public class CharAtom2 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm cha = params.getNth(1);
        JIPTerm atom = params.getNth(2);

        // check if input is a variable
        if (cha instanceof JIPVariable)
        {
            // try to extract the term
            if(((JIPVariable)cha).isBounded())
            {
                //extracts the term
                cha = ((JIPVariable)cha).getValue();
            }
        }

        if((cha instanceof JIPNumber))
        {
            if(!((JIPNumber)cha).isInteger())
                throw new JIPParameterTypeException(1, JIPParameterTypeException.INTEGER);

            int c = (int)((JIPNumber)cha).getDoubleValue();
            if(c < 0 || c > 255)
                throw new JIPParameterTypeException(1, JIPParameterTypeException.INTEGER);

            //PrologObject term1 = atom;
            JIPTerm term = JIPAtom.create("" + (char)c);
            return atom.unify(term, varsTbl);
        }
        else
        {
            if (atom instanceof JIPVariable)
            {
                // try to extract the term
                if(((JIPVariable)atom).isBounded())
                {
                    //extracts the term
                    atom = ((JIPVariable)atom).getValue();
                }
            }

            if(!(atom instanceof JIPAtom))
                throw new JIPParameterTypeException(2, JIPParameterTypeException.ATOM);

            String strAtom = ((JIPAtom)atom).getName();
            if(strAtom.length() > 1)
                throw new JIPParameterTypeException(2, JIPParameterTypeException.ATOM);

            return cha.unify(JIPNumber.create(strAtom.charAt(0)), varsTbl);
        }
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

