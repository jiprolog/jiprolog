/*
 * 09/19/2002
 *
 * Copyright (C) 2002 Ugo Chirico
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

package com.ugos.jiprolog.extensions.exception;

import java.util.Hashtable;

import com.ugos.jiprolog.engine.JIPCons;
import com.ugos.jiprolog.engine.JIPVariable;
import com.ugos.jiprolog.engine.JIPXCall;

public class JIPThrow1 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable<JIPVariable, JIPVariable> varsTbl)
    {
        throw new JIPUserException(params.getNth(1));
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

