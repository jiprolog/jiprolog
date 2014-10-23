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

//create_object('java.lang.String'('java.lang.String'), ['ugo e ornella'], X).

public class JIPCreateObject3 extends JIPXCall
{
    //private JIPAtom m_handle;

    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm className = params.getNth(1);
        JIPTerm paramList = params.getNth(2);
        JIPTerm handle    = params.getNth(3);

        // check if className is a variable
        if (className instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)className).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                className = ((JIPVariable)className).getValue();
            }
        }

        if(!(className instanceof JIPAtom) && !(className instanceof JIPFunctor))
            throw new JIPRuntimeException(JIPxReflect.ERR_UNEXPECTED_TERM, JIPxReflect.STR_UNEXPECTED_TERM);

        // check if paramList  is a variable
        if (paramList instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)paramList ).isBounded())
            {
                throw new JIPParameterUnboundedException(2);
            }
            else
            {
                //extracts the term
                paramList  = ((JIPVariable)paramList ).getValue();
            }
        }

        if(!(paramList instanceof JIPList))
            throw new JIPRuntimeException(JIPxReflect.ERR_UNEXPECTED_TERM, JIPxReflect.STR_UNEXPECTED_TERM);

        // check if handle is a variable
        if (!(handle instanceof JIPVariable) || ((JIPVariable)handle).isBounded())
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_UNEXPECTED_TERM, JIPxReflect.STR_UNEXPECTED_TERM);
        }

        try
        {
            if(className.toString().startsWith("["))
            {
                throw new JIPRuntimeException(JIPxReflect.ERR_UNEXPECTED_TERM, "Unable to create an array: " + className);
            }

            // Prepare params
            Vector objVect = new Vector();
            JIPList listParam = (JIPList)paramList;

            while(listParam != null && listParam.getHead() != null)
            {
                JIPTerm term = listParam.getHead();
                Object marshalledTerm = JIPxReflect.marshallIn(term);

                objVect.addElement(marshalledTerm);

                // next term
                if(listParam.getTail() instanceof JIPVariable)
                {
                    listParam = (JIPList)((JIPVariable)listParam.getTail()).getValue();
                }
                else
                {
                    listParam = (JIPList)listParam.getTail();
                }
            }

            // get the class
            Class objClass;
            if(className instanceof JIPAtom)
            {
                // get the class
                objClass = getClass().forName(((JIPAtom)className).getName());
            }
            else
            {
                // get the class
                objClass = getClass().forName(((JIPFunctor)className).getName().toString());

            }

            Object paramObj[]  = new Object[objVect.size()];
            objVect.copyInto(paramObj);

            Class[] paramsClass = JIPxReflect.getParamsClass(className);
            // get the rigth constructor
            Constructor constr = objClass.getConstructor(paramsClass);

            // create new instance
            Object obj = constr.newInstance(paramObj);

            //Store object
            JIPTerm thandle = JIPxReflect.putObject(obj);

            return handle.unify(thandle, varsTbl);
        }
        catch(ClassNotFoundException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_CLASS_NOT_FOUND, JIPxReflect.STR_CLASS_NOT_FOUND);
        }
        catch(ClassCastException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_CLASS_CAST, JIPxReflect.STR_CLASS_CAST);
        }
        catch(NoSuchMethodException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_METHOD_NOT_FOUND, JIPxReflect.STR_METHOD_NOT_FOUND);
        }
        catch(InvocationTargetException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_INSTANTIATION, ex.getMessage());
        }
        catch(IllegalAccessException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_INSTANTIATION, ex.getMessage());
        }
        catch(InstantiationException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_INSTANTIATION, ex.getMessage());
        }
    }

    public final boolean hasMoreChoicePoints()
    {
        return false;
    }
}

