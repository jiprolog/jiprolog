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

public class JIPInvoke4 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm handle     = params.getNth(1);
        JIPTerm methodProto = params.getNth(2);
        JIPTerm paramList  = params.getNth(3);
        JIPTerm retVal     = params.getNth(4);

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

        // check if className is a variable
        if (methodProto instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)methodProto).isBounded())
            {
                throw new JIPRuntimeException(JIPxReflect.ERR_UNBOUNDED, JIPxReflect.STR_UNBOUNDED);
            }
            else
            {
                //extracts the term
                methodProto = ((JIPVariable)methodProto).getValue();
            }
        }

        if(!(methodProto instanceof JIPAtom) && !(methodProto instanceof JIPFunctor))
            throw new JIPRuntimeException(JIPxReflect.ERR_UNEXPECTED_TERM, JIPxReflect.STR_UNEXPECTED_TERM);

        // check if paramList  is a variable
        if (paramList instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)paramList ).isBounded())
            {
                throw new JIPRuntimeException(JIPxReflect.ERR_UNBOUNDED, JIPxReflect.STR_UNBOUNDED);
            }
            else
            {
                //extracts the term
                paramList  = ((JIPVariable)paramList ).getValue();
            }
        }


        if(!(paramList instanceof JIPList))
            throw new JIPRuntimeException(JIPxReflect.ERR_UNEXPECTED_TERM, JIPxReflect.STR_UNEXPECTED_TERM);

        try
        {
            Vector objVect = new Vector();
            JIPList listParam = (JIPList)paramList;

            while(listParam != null && listParam.getHead() != null)
            {
                JIPTerm term = listParam.getHead();
                Object marshalledTerm = JIPxReflect.marshallIn(term);

//                if(marshalledTerm instanceof Integer)
//                    classVect.addElement(((Integer)marshalledTerm).TYPE);
//                else if(marshalledTerm instanceof Double)
//                    classVect.addElement(((Double)marshalledTerm).TYPE);
//                else if(marshalledTerm instanceof Boolean)
//                    classVect.addElement(((Boolean)marshalledTerm).TYPE);
//                else
//                    classVect.addElement(marshalledTerm.getClass());

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

            Object paramObj[]  = new Object[objVect.size()];
            objVect.copyInto(paramObj);

            Method method;
            Object objRetVal;
            Class[] paramsClass;
            String strMethodName =
                (methodProto instanceof JIPAtom)
                    ? ((JIPAtom)methodProto).getName() : ((JIPFunctor)methodProto).getName();

            String atomHandle = ((JIPAtom)handle).getName();

            //System.out.println("handle.toString() " + handle.toString());
            if(atomHandle.startsWith("#"))
            {
                // get the object
                Object obj = JIPxReflect.getObject(atomHandle);
                if(obj == null)
                    throw new JIPRuntimeException(JIPxReflect.ERR_OBJECT_NOT_FOUND, JIPxReflect.STR_OBJECT_NOT_FOUND);

                paramsClass = JIPxReflect.getParamsClass(methodProto);

                //method = getMethod(obj.getClass(), methodProto);
                // get the rigth method
                method = obj.getClass().getMethod(strMethodName, paramsClass);

                // invoke method
                objRetVal = method.invoke(obj, paramObj);
            }
            else
            {
                String strClassname = atomHandle;
//              elimina eventuali apici
                if(strClassname.charAt(0) == 39 || strClassname.charAt(0) == 34)
                {
                    strClassname = strClassname.substring(1, strClassname.length() - 1);
                }

                //System.out.println(strClassname);

                // get the class
                Class objClass = getClass().forName(strClassname);

                paramsClass = JIPxReflect.getParamsClass(methodProto);

                //method = getMethod(objClass, methodProto);
                // get the rigth method
                //method = objClass.getMethod(methodName.toString(), paramClass);
                method = objClass.getMethod(strMethodName, paramsClass);

                // invoke method
                objRetVal = method.invoke(null, paramObj);
            }

            //Store object
            JIPTerm retVal1 = JIPxReflect.marshallOut(objRetVal);

            return retVal.unify(retVal1, varsTbl);
        }
        catch(ClassCastException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_CLASS_CAST, JIPxReflect.STR_CLASS_CAST);
        }
        catch(ClassNotFoundException ex)
        {
            throw new JIPRuntimeException(JIPxReflect.ERR_CLASS_NOT_FOUND, JIPxReflect.STR_CLASS_NOT_FOUND);
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
    }

    public final boolean hasMoreChoicePoints()
    {
        return false;
    }
}

