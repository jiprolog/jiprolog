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

import java.io.IOException;
import java.util.Enumeration;
import java.util.Hashtable;

import com.ugos.jiprolog.engine.JIPAtom;
import com.ugos.jiprolog.engine.JIPCons;
import com.ugos.jiprolog.engine.JIPInstantiationException;
import com.ugos.jiprolog.engine.JIPNumber;
import com.ugos.jiprolog.engine.JIPTerm;
import com.ugos.jiprolog.engine.JIPXCall;

public final class EOF2 extends JIPXCall
{
    public final boolean unify(final JIPCons params, final Hashtable varsTbl)
    {
        JIPTerm handle = params.getNth(1).getValue();
        if(handle == null)
            throw new JIPInstantiationException(2);

        // Get the stream
        InputStreamInfo sinfo = (InputStreamInfo)JIPio.getStreamInfo(handle);
       	if(sinfo == null)
       		return false;

       	try
       	{
			return sinfo.isEOF();
		}
       	catch (IOException e)
       	{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

       	return false;
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

