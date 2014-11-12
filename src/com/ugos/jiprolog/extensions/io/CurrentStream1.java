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

import java.util.Enumeration;
import java.util.Hashtable;

import com.ugos.jiprolog.engine.JIPAtom;
import com.ugos.jiprolog.engine.JIPCons;
import com.ugos.jiprolog.engine.JIPXCall;

public final class CurrentStream1 extends JIPXCall
{
    private Enumeration m_ienum = null;
    private Enumeration m_oenum = null;

    public final boolean unify(final JIPCons params, final Hashtable varsTbl)
    {
        if(m_ienum == null)
        {
            m_ienum = JIPio.itable.elements();
        }

        if(m_oenum == null)
        {
            m_oenum = JIPio.otable.elements();
        }

        while(m_ienum.hasMoreElements())
        {
            StreamInfo sinfo = (StreamInfo)m_ienum.nextElement();
            JIPAtom handle = JIPAtom.create(sinfo.getHandle());

            JIPCons cons = JIPCons.create(handle, null);

            if(params.unifiable(cons))
            	return params.unify(cons, varsTbl);
        }

        while(m_oenum.hasMoreElements())
        {
            StreamInfo sinfo = (StreamInfo)m_oenum.nextElement();
            JIPAtom handle = JIPAtom.create(sinfo.getHandle());

            JIPCons cons = JIPCons.create(handle, null);
            if(params.unifiable(cons))
            	return params.unify(cons, varsTbl);
        }

        return false;
    }

    public boolean hasMoreChoicePoints()
    {
        return m_ienum.hasMoreElements() || m_oenum.hasMoreElements();
    }
}

