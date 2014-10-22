/*
 * Copyright (C) 1999-2004 By Ugo Chirico
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

public class DeleteFile1 extends JIPXCall
{
    public final boolean unify(final JIPCons params, final Hashtable varsTbl)
    {
        JIPTerm input = params.getNth(1);
        
        // check if input is a variable
        if (input instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)input).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                input = ((JIPVariable)input).getValue();
            }
        }
        
        // check if input is a Number
        if(input instanceof JIPAtom)
        {
            String strPath = ((JIPAtom)input).getName();
            File ffile = new File(strPath);
            if(!ffile.isAbsolute())
            {
                ffile = new File(getJIPEngine().getSearchPath() + File.separator + strPath);
                strPath = ffile.getAbsolutePath();
            }

            if(!ffile.exists())
                throw new JIPRuntimeException(6, strPath);
            
            if(!ffile.delete())
                throw new JIPRuntimeException(JIPio.ERR_FILE_NOT_DELETED, JIPio.STR_FILE_NOT_DELETED);
        }
        else
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM);
        }
        
        return true;
    }
    
    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

