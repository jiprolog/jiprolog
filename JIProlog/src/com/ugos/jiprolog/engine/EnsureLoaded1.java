/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico - http://www.ugochirico.com
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

package com.ugos.jiprolog.engine;

import java.io.*;
import java.util.*;

final class EnsureLoaded1 extends BuiltIn
{
    public final boolean unify(final Hashtable varsTbl)
    {
        // try if object file

        String strPath = null;
        PrologObject path = getRealTerm(getParam(1));

        if(path instanceof Atom)
        {
            strPath = ((Atom)path).getName();
        }
        else if(path instanceof PString)
        {
            strPath = ((PString)path).getString();
        }
        else
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM_OR_STRING);
        }

        try
        {
        	if(strPath.toLowerCase().endsWith(".jip"))
        		Load1.load(strPath, getJIPEngine());
        	else
        		Consult1.consult(strPath, getJIPEngine(), getQueryHandle());
        }
        catch(Exception ex1)
        {
//                ex1.printStackTrace();
            throw new JIPJVMException(ex1);
        }

        return true;
    }
}
