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

public final class CurrentStream3 extends JIPXCall
{
    private Enumeration m_enum = null;
    
    public final boolean unify(final JIPCons params, final Hashtable varsTbl)
    {
        if(m_enum == null)
            m_enum = ((Hashtable)getJIPEngine().getEnvVariable("_iotable_")).elements();
                
        while(m_enum.hasMoreElements())
        {
            JIPio.StreamInfo sinfo = (JIPio.StreamInfo)m_enum.nextElement();
            JIPAtom object = JIPAtom.create(sinfo.m_strName);
            JIPAtom mode = (sinfo.m_stream instanceof InputStream) ? JIPAtom.create("read") : JIPAtom.create("write");
            JIPAtom handle = JIPAtom.create(sinfo.m_handle);
            
            JIPCons cons = JIPCons.create(object, JIPCons.create(mode, JIPCons.create(handle, null)));
            return params.unify(cons, varsTbl);
        }
        
        return false;
    }
        
    public boolean hasMoreChoicePoints()
    {
        return m_enum.hasMoreElements();
    }
}

