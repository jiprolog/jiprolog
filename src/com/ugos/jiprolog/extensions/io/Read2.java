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

public final class Read2 extends JIPXCall
{
    private Enumeration m_termEnum;
    private boolean     m_bEOF;
    private String      m_strStreamHandle;

    private StreamInfo streamInfo;
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        if(m_bEOF)
            return false;

        if(m_termEnum == null) // First time
        {
            // get first parameter
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

            // check if input is an Atom
            if(input instanceof JIPAtom)
            {
                // Gets the handle to the stream
                m_strStreamHandle = ((JIPAtom)input).getName();

                streamInfo = JIPio.getStreamInfo(m_strStreamHandle);
                if(streamInfo == null)
                {
                	throw JIPExistenceException.createStreamException(JIPAtom.create(m_strStreamHandle));
                }

                // Get the stream
                m_termEnum = JIPio.getTermEnumeration(m_strStreamHandle, getJIPEngine());
                if(m_termEnum == null)
                {
                	throw JIPExistenceException.createSourceSynkException(JIPAtom.create(m_strStreamHandle));
                }
            }
            else
            {
                throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM);
            }
        }

        boolean bUserStream;
        if(bUserStream = "user_input".equals(m_strStreamHandle))
            getJIPEngine().notifyEvent(JIPEvent.ID_WAITFORUSERINPUT, getPredicate(), getQueryHandle());

        JIPTerm term;
        try
        {
            if(m_termEnum.hasMoreElements())
            {
                term = (JIPTerm)m_termEnum.nextElement();

            }
            else
            {
                if(bUserStream)
                {
                    term = JIPCons.NIL;
                }
                else
                {
                    m_bEOF = true;
                    term = JIPAtom.create("end_of_file");
                    streamInfo.setEndOfStream("past");
                }
            }
        }
        catch(JIPRuntimeException ex)
        {
            if(bUserStream)
                getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

            throw ex;
        }

        if(bUserStream)
            getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

        // true if the term read unify with the second paramater
        return params.getNth(2).unify(term, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

