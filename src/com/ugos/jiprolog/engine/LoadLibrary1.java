/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico
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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

final class LoadLibrary1 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        String strPath;
        final PrologObject param = getRealTerm(getParam(1));

        if (param instanceof Atom)
        {
            // try without protocol
            strPath = ((Atom)param).getName();
        }
        else if (param instanceof PString)
        {
            // try without protocol
            strPath = ((PString)param).getString();
        }
        else
        {
            throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, param);
        }

        if(strPath.charAt(0) == 39 || strPath.charAt(0) == 34)
        {
            strPath = strPath.substring(1, strPath.length() - 1);
        }

        //System.out.println(m_strSearchDir);
//        strPath = strPath.replace('\\', '/');

        //System.out.println(m_strSearchDir);
        String strFileName[] = new String[1];
        String strCurDir[]   = new String[1];

        try
        {
            final InputStream ins = StreamManager.getStreamManager().getInputStream(strPath, getJIPEngine().getSearchPath(), strFileName, strCurDir);
            ins.close();
            getJIPEngine().loadLibrary(strFileName[0]);
        }
        catch(FileNotFoundException ex)
        {
        	throw JIPExistenceException.createSourceSynkException(Atom.createAtom(strPath));
//            throw JIPRuntimeException.create(6, strPath);
        }
        catch(IOException ex)
        {
            throw new JIPJVMException(ex);
        }
        catch(SecurityException ex)
        {
        	throw new JIPPermissionException("access", "source_sink", Atom.createAtom(strPath));
//            throw JIPRuntimeException.create(9, null);
        }

        return true;
    }
}
