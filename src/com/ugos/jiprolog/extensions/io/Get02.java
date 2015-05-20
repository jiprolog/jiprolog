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

public final class Get02 extends JIPXCall
{
    protected final int readNextChar(InputStream ins)
    {
        try
        {
            return ins.read();
        }
        catch(IOException ex)
        {
            throw new JIPJVMException(ex);
        }
    }

    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        // get first parameter
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

        StreamInfo streamInfo = JIPio.getStreamInfo(input);

        Properties properties = streamInfo.getProperties();
        if(!(properties.getProperty("mode").equals("mode(read)")))
        	throw new JIPPermissionException("input", "stream", input);

        int streamHandle = streamInfo.getHandle();

        // Get the stream
        final InputStream ins = JIPio.getInputStream(streamHandle, getJIPEngine());

        if(streamHandle == JIPEngine.USER_INPUT_HANDLE)
            getJIPEngine().notifyEvent(JIPEvent.ID_WAITFORUSERINPUT, getPredicate(), getQueryHandle());

        JIPTerm term;
        int c = readNextChar(ins);
        if(c == -1)
        {
        	streamInfo.setEndOfStream("past");
        }

        term = JIPNumber.create(c);

        if(streamHandle == JIPEngine.USER_INPUT_HANDLE)
            getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

        return params.getNth(2).unify(term, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

