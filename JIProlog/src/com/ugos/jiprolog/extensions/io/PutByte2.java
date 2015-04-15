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

public class PutByte2 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm output = params.getNth(1);
        JIPTerm b = params.getNth(2);

        // check if input is a variable
        if (output instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)output).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                output = ((JIPVariable)output).getValue();
            }
        }

        if(!(output instanceof JIPAtom))
            throw new JIPTypeException(JIPTypeException.ATOM, output);

        // check if input is a variable
        if (b instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)b).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                b = ((JIPVariable)b).getValue();
            }
        }

        if(!(b instanceof JIPNumber))
            throw new JIPTypeException(JIPTypeException.INTEGER, b);


        JIPAtom   handle = (JIPAtom)output;
        int nCode = (int)((JIPNumber)b).getDoubleValue();

        // Gets the handle to the stream
        String strStreamHandle = (handle).getName();
        OutputStream writer;

        OutputStreamInfo sinfo = (OutputStreamInfo)JIPio.getStreamInfo(strStreamHandle);
        String mode = sinfo.getProperties().getProperty("mode");
        if(!(mode.equals("mode(write)") || mode.equals("mode(append)")))
        	throw new JIPPermissionException("output", "stream", output);

        if(!sinfo.getProperties().getProperty("type").equals("type(binary)"))
        	throw new JIPPermissionException("output", "text_stream", output);

        // Get the stream
        writer = JIPio.getOutputStream(strStreamHandle, getJIPEngine());
        if(writer == null)
        {
        	throw JIPExistenceException.createStreamException(strStreamHandle);
//        	throw new JIPDomainException("stream_or_alias", strStreamHandle);
        }

        if(nCode < -1 || nCode > 255)  // ignore all
        	throw new JIPTypeException(JIPTypeException.BYTE, b);

        try
        {
            writer.write((byte)nCode);
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

