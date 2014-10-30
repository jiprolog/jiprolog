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

import com.ugos.io.PushBackInputStream;
import com.ugos.jiprolog.engine.*;
import com.ugos.jiprolog.extensions.io.JIPio.StreamInfo;

import java.io.*;
import java.util.*;

public final class CurrentStream4 extends JIPXCall
{
    private Enumeration m_enum = null;

    public final boolean unify(final JIPCons params, final Hashtable varsTbl)
    {
        if(m_enum == null)
        {
        	Hashtable<String, StreamInfo> iotable = JIPio.iotable;
            m_enum = iotable.elements();
        }

        while(m_enum.hasMoreElements())
        {
            JIPio.StreamInfo sinfo = (JIPio.StreamInfo)m_enum.nextElement();
            JIPAtom object = JIPAtom.create(sinfo.m_strName);
            JIPAtom mode = (sinfo.m_stream instanceof InputStream) ? JIPAtom.create("read") : JIPAtom.create("write");
            JIPAtom handle = JIPAtom.create(sinfo.m_handle);
            JIPNumber position;
            if(sinfo.m_stream instanceof InputStream)
            {
//            	position = JIPNumber.create(((PushBackInputStream)sinfo.m_stream).getLineNumber());
            	if(sinfo.m_enum != null)
            		position = JIPNumber.create(((StreamPosition)sinfo.m_enum).getLineNumber());
            	else
            		position = JIPNumber.create(0);
            }
            else
            {
            	position = JIPNumber.create(0);
            }

            //open('C:\\Progetti\\Applicazioni\\JIProlog\\JIProlog\\src\\com\\ugos\\jiprolog\\resources\\xio.pl', read, H).
            //xcall('com.ugos.jiprolog.extensions.io.CurrentStream4', ['#16430513', _A, _B, P]).
//            JIPCons cons = JIPCons.create(position, JIPCons.create(object, JIPCons.create(mode, JIPCons.create(handle, null))));

            JIPCons cons = JIPCons.create(handle, JIPCons.create(mode, JIPCons.create(object, JIPCons.create(position, null))));

            return params.unify(cons, varsTbl);
        }

        return false;
    }

    public boolean hasMoreChoicePoints()
    {
        return m_enum.hasMoreElements();
    }
}

