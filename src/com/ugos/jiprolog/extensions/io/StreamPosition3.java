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

import com.ugos.io.PushbackLineNumberInputStream;
import com.ugos.jiprolog.engine.*;

import java.io.*;
import java.util.*;

public final class StreamPosition3 extends JIPXCall
{
    public final boolean unify(final JIPCons params, final Hashtable varsTbl)
    {
       	Hashtable<String, InputStreamInfo> itable = JIPio.itable;

        JIPTerm handle = params.getNth(1).getValue();
        if(handle == null)
            throw new JIPInstantiationException(2);

    	String shandle = ((JIPAtom)handle).getName();

       	InputStreamInfo sinfo = itable.get(shandle);

       	JIPCons cons;
       	if(sinfo == null)
       		cons = JIPCons.create(handle, JIPCons.create(JIPNumber.create(0), JIPCons.create(JIPNumber.create(0), null)));
       	else
       		cons = JIPCons.create(handle, JIPCons.create(JIPNumber.create(sinfo.getPosition()), JIPCons.create(JIPNumber.create(sinfo.getLineNumber()), null)));

        return params.unify(cons, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

