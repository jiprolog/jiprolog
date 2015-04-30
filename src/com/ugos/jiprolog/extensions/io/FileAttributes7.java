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

public final class FileAttributes7 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm file = params.getNth(1);

        // check if input is a variable
        if (file instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)file).isBounded())
            {
                throw new JIPInstantiationException(1);
            }
            else
            {
                //extracts the term
                file = ((JIPVariable)file).getValue();
            }
        }

        if(!(file instanceof JIPAtom))
            throw new JIPTypeException(JIPTypeException.ATOM, file);

        String strFileName = ((JIPAtom)file).getName();
        File ffile = new File(strFileName);
        if(!ffile.isAbsolute())
        {
            //System.out.println("absolute");
            ffile = new File(getJIPEngine().getSearchPath() + File.separator + strFileName);
        }

        JIPTerm ext;
        JIPAtom dir;
        //JIPNumber size, time;

        try
        {
            String strName = ffile.getName();
            int nPos = strName.lastIndexOf('.');
            if(nPos > -1)
            {
                ext  = JIPAtom.create(strName.substring(nPos + 1));
                strName = strName.substring(0, nPos);
            }
            else
            {
                ext  = JIPList.NIL;
            }

            // name
            if(!params.getNth(2).unify(JIPAtom.create(strName), varsTbl))
                return false;

            // extension
            if(!params.getNth(3).unify(ext, varsTbl))
                return false;

            // dir
            nPos = ffile.getCanonicalPath().lastIndexOf(File.separatorChar);
            if(nPos > -1)
            {
                dir = JIPAtom.create(ffile.getCanonicalPath().substring(0, nPos));
            }
            else
            {
                dir = JIPAtom.create(ffile.getCanonicalPath());
            }

            if(!params.getNth(4).unify(dir, varsTbl))
                return false;

            // absolute path
            if(!params.getNth(5).unify(JIPAtom.create(ffile.getCanonicalPath()), varsTbl))
                return false;

            // size
            if(!params.getNth(6).unify(JIPNumber.create(ffile.length()), varsTbl))
                return false;

            // time
            if(!params.getNth(7).unify(JIPNumber.create(ffile.lastModified()), varsTbl))
                return false;

            return true;
        }
        catch(IOException ex)
        {
            throw new JIPJVMException(ex);
        }
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

