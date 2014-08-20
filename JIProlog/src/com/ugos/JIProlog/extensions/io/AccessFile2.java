/*
 *
 * Copyright (C) 1999-2004 Ugo Chirico
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

package com.ugos.JIProlog.extensions.io;

import com.ugos.JIProlog.engine.*;

import java.io.*;
import java.util.*;

public final class AccessFile2 extends JIPXCall
{
    //JIPAtom m_mode;
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm file = params.getNth(1);
        JIPTerm mode = params.getNth(2);
        
        // check if input is a variable
        if (file instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)file).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                file = ((JIPVariable)file).getValue();
            }
        }
        
        // check if input is a variable
        if (mode instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)mode).isBounded())
            {
                throw new JIPParameterUnboundedException(2);
            }
            else
            {
                //extracts the term
                mode = ((JIPVariable)mode).getValue();
            }
        }
        
        if(!(file instanceof JIPAtom))
            throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM);
        
        if(!(mode instanceof JIPAtom))
            throw new JIPParameterTypeException(2, JIPParameterTypeException.ATOM);
        
        String strFileName = ((JIPAtom)file).getName();
        
        File ffile = new File(strFileName);
        //System.out.println("strFileName " + strFileName);
        if(!ffile.isAbsolute())
        {
            ffile = new File(getJIPEngine().getSearchPath() + File.separator + strFileName);
        }
        
        //System.out.println("getAbsolutePath() " + ffile.getAbsolutePath());
        
        if(ffile.canWrite() && (mode.unify(JIPAtom.create("write"), varsTbl)))
        {
//          System.out.println("write");
            return true;
        }
        else if(ffile.canWrite() && mode.unify(JIPAtom.create("append"), varsTbl))
        {
//          System.out.println("append");
            return true;
        }
        else if(ffile.canRead() && mode.unify(JIPAtom.create("read"), varsTbl))
        {
//          System.out.println("read");
            return true;
        }
        else if(ffile.exists() && mode.unify(JIPAtom.create("exist"), varsTbl))
        {
//          System.out.println("exist");
            return true;
        }
        else if(ffile.isDirectory() && mode.unify(JIPAtom.create("directory"), varsTbl))
        {
//          System.out.println("directory");
            return true;
        }
        
        
//      System.out.println("none");
        return false;
    }
       
    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

