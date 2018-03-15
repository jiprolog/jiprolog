/*
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

package com.ugos.jiprolog.extensions.io;

import com.ugos.jiprolog.engine.*;

import java.io.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class AbsoluteFileName2 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm file = params.getNth(1);
        JIPTerm abs = params.getNth(2);

        // check if input is a variable
        if (file instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)file).isBounded())
            {
                throw new JIPInstantiationException(1);
            }
            else
            {
                //extracts the term
                file = ((JIPVariable)file).getValue();
            }
        }

        if(!(file instanceof JIPAtom))
            throw new JIPTypeException(JIPTypeException.ATOM, file);

        String strFileName = ((JIPAtom)file).getName();
        strFileName = JIPio.resolvePath(strFileName);
        File ffile = new File(strFileName);

        String absolutePath;
        if(ffile.isAbsolute())
        {
        	absolutePath = ffile.getAbsolutePath();
        		        
        }
        else
        {
        	absolutePath = new File(getJIPEngine().getSearchPath(), strFileName).getAbsolutePath();
        }
                	
        return abs.unify(JIPAtom.create(absolutePath), varsTbl);
    }

    
 
    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

