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

public final class GetByte2 extends JIPXCall
{
    private int streamHandle;

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
        JIPTerm b = params.getNth(2);

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

        // check if input is an Atom
        if(input instanceof JIPNumber)
        {
            // Gets the handle to the stream
            streamHandle = (int)((JIPNumber)input).getDoubleValue();

            // Get the stream

	        StreamInfo sinfo = JIPio.getStreamInfo(streamHandle);
	        if(sinfo == null)
            	throw JIPExistenceException.createStreamException(JIPNumber.create(streamHandle));

	        Properties properties = sinfo.getProperties();
	        if(!(properties.getProperty("mode").equals("mode(read)")))
	        	throw new JIPPermissionException("input", "stream", input);
	        if(!properties.getProperty("type").equals("type(binary)"))
	        	throw new JIPPermissionException("input", "text_stream", input);

	        final InputStream ins = JIPio.getInputStream(streamHandle, getJIPEngine());
            if(ins == null)
            {
            	throw JIPExistenceException.createStreamException(JIPNumber.create(streamHandle));
//            	throw new JIPDomainException("stream_or_alias", m_strStreamHandle);
            }

	        if (b instanceof JIPVariable && ((JIPVariable)b).isBounded())
	        {
                b = ((JIPVariable)b).getValue();
		        if(!(b instanceof JIPNumber))
		            throw new JIPTypeException(JIPTypeException.IN_BYTE, b);

		        int nCode = (int)((JIPNumber)b).getDoubleValue();
		        if(nCode < -1 || nCode > 255)
		            throw new JIPTypeException(JIPTypeException.IN_BYTE, b);
	        }

			if(properties.getProperty("end_of_stream").equals("end_of_stream(past)"))
			{
				if(properties.getProperty("eof_action").equals("eof_action(error)"))
					throw new JIPPermissionException("input", "past_end_of_stream", JIPNumber.create(streamHandle));
				else if(properties.getProperty("eof_action").equals("eof_action(eof_code)"))
		            return params.getNth(2).unify(JIPNumber.create(-1), varsTbl);
				else // eof_action(reset)
					return unify(params, varsTbl);
			}
			else if(properties.getProperty("end_of_stream").equals("end_of_stream(at)")) {
				sinfo.setEndOfStream("past");
	            return params.getNth(2).unify(JIPNumber.create(-1), varsTbl);
			}
			else
			{ // end_of_stream(not)
	            if("user_input".equals(streamHandle))
	                getJIPEngine().notifyEvent(JIPEvent.ID_WAITFORUSERINPUT, getPredicate(), getQueryHandle());

	            int c = readNextChar(ins);
	            if(c == -1)
	            {
	            	sinfo.setEndOfStream("past");
	            }


				JIPTerm term = JIPNumber.create(c);

	            if("user_input".equals(streamHandle))
	                getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

	            return params.getNth(2).unify(term, varsTbl);
			}
        }
        else
            throw new JIPTypeException(JIPTypeException.NUMBER, params.getNth(2));
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

