/*
 * 09/19/2002
 *
 * Copyright (C) 1999-2003 Ugo Chirico
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

public final class FlushOutput1 extends JIPXCall
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
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                input = ((JIPVariable)input).getValue();
            }
        }

        if(!(input instanceof JIPAtom))
            throw new JIPTypeException(JIPTypeException.ATOM, input);

        String strStreamHandle = ((JIPAtom)input).getName();
        OutputStream writer;

    	StreamInfo streamInfo = JIPio.getStreamInfo(strStreamHandle);
    	if(streamInfo == null)
        	throw JIPExistenceException.createStreamException(JIPAtom.create(strStreamHandle));

    	Properties props = streamInfo.getProperties();
        if(!(props.getProperty("mode", "").equals("mode(append)")) &&
           !(props.getProperty("mode", "").equals("mode(write)")))
            throw new JIPPermissionException("output", "stream", input);

        // Get the stream
        writer = JIPio.getOutputStream(strStreamHandle, getJIPEngine());
        if(writer == null)
        {
        	throw JIPExistenceException.createStreamException(JIPAtom.create(strStreamHandle));
        }

        try
        {
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

