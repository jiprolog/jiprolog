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

public final class See2 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm input = params.getNth(1);

        // check if input is a variable
        if (input instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)input).isBounded())
            {
                throw new JIPInstantiationException(1);
            }
            else
            {
                //extracts the term
                input = ((JIPVariable)input).getValue();
            }
        }

        String strFilePath;
        // check if input is an atom
        if(input instanceof JIPAtom)
        {
            strFilePath = ((JIPAtom)input).getName();
        }
        else if(input instanceof JIPString)
        {
            strFilePath = ((JIPString)input).getStringValue();
        }
        else
        {
            throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, input);
        }

        // delete ' at the beggining and end of string
        if(strFilePath.charAt(0) == 39 || strFilePath.charAt(0) == 34)
        {
            strFilePath = strFilePath.substring(1, strFilePath.length() - 1);
        }

        //strFilePath = strFilePath.replace((char)92, File.separatorChar);
        //strFilePath = strFilePath.replace('/', File.separatorChar);

        JIPTerm handle = params.getNth(2);
        // check if handle is a variable
        if (handle instanceof JIPVariable)
        {
            // try to extract the term
            if(((JIPVariable)handle).isBounded())
            {
                handle = ((JIPVariable)handle).getValue();
            }
        }

        String strStreamHandle = null;

        // check if handle is an atom
        if(handle instanceof JIPAtom)
        {
            strStreamHandle = ((JIPAtom)handle).getName();
        }

        try
        {
            strStreamHandle = JIPio.openInputStream(strFilePath, strStreamHandle, getJIPEngine());
        }
        catch(FileNotFoundException ex)
        {
            //System.out.println("UGO " + ex);
            throw JIPExistenceException.createSourceSynkException(strFilePath);
        }
        catch(IOException ex)
        {
            //System.out.println("UGO " + ex);
            throw new JIPRuntimeException(JIPio.ERR_IOEXCEPTION, ex.toString());
        }

        if(strStreamHandle == null)
        	throw JIPExistenceException.createStreamException(strFilePath);


        // unify with the second parameter
        return params.getNth(2).unify(JIPAtom.create(strStreamHandle), varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

