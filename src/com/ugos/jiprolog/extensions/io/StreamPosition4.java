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

import java.util.Hashtable;

import com.ugos.jiprolog.engine.JIPAtom;
import com.ugos.jiprolog.engine.JIPCons;
import com.ugos.jiprolog.engine.JIPExistenceException;
import com.ugos.jiprolog.engine.JIPFunctor;
import com.ugos.jiprolog.engine.JIPInstantiationException;
import com.ugos.jiprolog.engine.JIPNumber;
import com.ugos.jiprolog.engine.JIPTerm;
import com.ugos.jiprolog.engine.JIPXCall;

public final class StreamPosition4 extends JIPXCall
{
    public final boolean unify(final JIPCons params, final Hashtable varsTbl)
    {
       	Hashtable<Integer, InputStreamInfo> itable = JIPio.itable;

        JIPTerm handle = params.getNth(1).getValue();
        if(handle == null)
            throw new JIPInstantiationException(2);

    	int nhandle = (int)((JIPNumber)handle).getDoubleValue();

       	InputStreamInfo sinfo = itable.get(nhandle);

       	JIPCons cons;
       	if(sinfo != null)
       		cons = JIPCons.create(handle, JIPCons.create(JIPNumber.create(sinfo.getPosition()), JIPCons.create(JIPNumber.create(sinfo.getLineNumber()), JIPCons.create(JIPNumber.create(sinfo.getColumn()), null))));
       	else
//       	throw JIPExistenceException.createStreamException(JIPAtom.create(shandle));
   			cons = JIPCons.create(handle, JIPCons.create(JIPNumber.create(0), JIPCons.create(JIPNumber.create(0), JIPCons.create(JIPNumber.create(0), null))));

       	return params.unify(cons, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}
