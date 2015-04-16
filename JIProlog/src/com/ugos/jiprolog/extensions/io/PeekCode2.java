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

import java.io.*;
import java.util.*;

import com.ugos.io.*;
import com.ugos.jiprolog.engine.*;

public final class PeekCode2 extends JIPXCall
{
//    private JIPTerm m_term;

   private final int peekCode(PushBackInputStream ins)
    {
        try
        {
            int c = ins.read();
            ins.pushback();
            return c;
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
        JIPTerm code = params.getNth(2);

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

        // check if input is an Atom
        if(input instanceof JIPAtom)
        {
            // Gets the handle to the stream
            final String strStreamHandle = ((JIPAtom)input).getName();

            // Get the stream
            PushBackInputStream ins = JIPio.getInputStream(strStreamHandle, getJIPEngine());
            if(ins == null)
            {
            	throw JIPExistenceException.createStreamException(strStreamHandle);
//            	throw new JIPDomainException("stream_or_alias", strStreamHandle);
            }

        	StreamInfo streamInfo = JIPio.getStreamInfo(strStreamHandle);
			Properties properties = streamInfo.getProperties();
	        if(!(properties.getProperty("mode").equals("mode(read)")))
	        	throw new JIPPermissionException("input", "stream", input);
	        if(!(properties.getProperty("type").equals("type(text)")))
	        	throw new JIPPermissionException("input", "binary_stream", input);

	        if (code instanceof JIPVariable && ((JIPVariable)code).isBounded())
	        {
                code = ((JIPVariable)code).getValue();
		        if(!(code instanceof JIPNumber))
		            throw new JIPTypeException(JIPTypeException.INTEGER, code);

		        int nCode = (int)((JIPNumber)code).getDoubleValue();
		        if(nCode == 0 || nCode < -1 || nCode > 255)
		        	throw new JIPRepresentationException("in_character_code");
	        }

			if(properties.getProperty("end_of_stream").equals("end_of_stream(past)")) {
				if(properties.getProperty("eof_action").equals("eof_action(error)"))
					throw new JIPPermissionException("input", "past_end_of_stream", JIPAtom.create(strStreamHandle));
				else if(properties.getProperty("eof_action").equals("eof_action(eof_code)"))
		            return params.getNth(2).unify(JIPNumber.create(-1), varsTbl);
				else // eof_action(reset)
					return unify(params, varsTbl);
			} else if(properties.getProperty("end_of_stream").equals("end_of_stream(at)")) {
	            return params.getNth(2).unify(JIPNumber.create(-1), varsTbl);
			} else { // end_of_stream(no)
				int c = peekCode(ins);
		        if(c == 0 || c < -1 || c > 255)
		        	throw new JIPRepresentationException("character");
				JIPTerm term = JIPNumber.create(c);
	            return params.getNth(2).unify(term, varsTbl);
			}
        }
        else
            throw new JIPTypeException(JIPTypeException.ATOM, params.getNth(2));
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

