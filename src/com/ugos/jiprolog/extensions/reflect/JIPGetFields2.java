/*
 * 28/03/2003
 *
 * Copyright (C) 1999-2003 Ugo Chirico
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


package com.ugos.jiprolog.extensions.reflect;

import com.ugos.jiprolog.engine.*;

import java.util.*;
import java.lang.reflect.*;

public class JIPGetFields2 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm handle     = params.getNth(1);
        JIPTerm fieldsList = params.getNth(2);

        // check if className is a variable
        if (handle instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)handle).isBounded())
            {
                throw new JIPInstantiationException(1);
            }
            else
            {
                //extracts the term
                handle = ((JIPVariable)handle).getValue();
            }
        }

        if(!(handle instanceof JIPAtom))
            throw new JIPRuntimeException(JIPxReflect.ERR_UNEXPECTED_TERM, JIPxReflect.STR_UNEXPECTED_TERM);

        try
        {
            Field[] fields;

            String atomHandle = ((JIPAtom)handle).getName();

            if(atomHandle.startsWith("#"))
            {
                // get the object
                Object obj = JIPxReflect.getObject(atomHandle);
                if(obj == null)
                    throw new JIPRuntimeException(JIPxReflect.ERR_OBJECT_NOT_FOUND, JIPxReflect.STR_OBJECT_NOT_FOUND);

                fields = obj.getClass().getFields();
            }
            else
            {
                fields = getClass().forName(atomHandle).getFields();
            }

            JIPList fieldsList1 = JIPList.NIL;
            for(int i = fields.length - 1; i >= 0; i--)
            {
                fieldsList1 = JIPList.create(JIPAtom.create(fields[i].getName()), fieldsList1);
            }

            return fieldsList.unify(fieldsList1, varsTbl);
        }
        catch(ClassCastException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_CLASS_CAST, JIPxReflect.STR_CLASS_CAST);
        }
        catch(ClassNotFoundException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_CLASS_NOT_FOUND, JIPxReflect.STR_CLASS_NOT_FOUND);
        }
    }

    public final boolean hasMoreChoicePoints()
    {
        return false;
    }
}

