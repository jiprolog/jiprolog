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

public class Writeq2 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm input = params.getNth(1);
        JIPTerm term = params.getNth(2);

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

        if(!(input instanceof JIPAtom))
            throw new JIPTypeException(JIPTypeException.ATOM, input);

        JIPAtom handle = (JIPAtom)input;

        // Gets the handle to the stream
        String strStreamHandle = (handle).getName();

        StreamInfo sinfo = (StreamInfo)JIPio.getStreamInfo(strStreamHandle);
        if(sinfo == null)
        	throw JIPExistenceException.createStreamException(JIPAtom.create(strStreamHandle));

        String mode = sinfo.getProperties().getProperty("mode");
        if(!(mode.equals("mode(write)") || mode.equals("mode(append)")))
        	throw new JIPPermissionException("output", "stream", input);
        if(!sinfo.getProperties().getProperty("type").equals("type(text)"))
        	throw new JIPPermissionException("output", "binary_stream", input);

        // Get the stream
        OutputStream writer = JIPio.getOutputStream(strStreamHandle, getJIPEngine());
        if(writer == null)
        {
        	throw JIPExistenceException.createStreamException(strStreamHandle);
//        	throw new JIPDomainException("stream_or_alias", strStreamHandle);
        }


        try
        {
            writer.write(term.toStringq(getJIPEngine()).getBytes(getJIPEngine().getEncoding()));
            writer.flush();
        }
        catch(IOException ex)
        {
            throw new JIPRuntimeException(JIPio.ERR_IOEXCEPTION, ex.getMessage());
        }

        return true;
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

