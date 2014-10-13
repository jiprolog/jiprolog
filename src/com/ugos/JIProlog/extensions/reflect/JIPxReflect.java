/*
 * 09/19/2002
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


package com.ugos.JIProlog.extensions.reflect;

import java.math.*;
import java.util.*;
import com.ugos.JIProlog.engine.*;

public class JIPxReflect
{
    public static final int    ERR_UNBOUNDED = 2101;
    public static final String STR_UNBOUNDED = "Unexpected unbounded variable found";

    public static final int    ERR_UNEXPECTED_TERM = 2102;
    public static final String STR_UNEXPECTED_TERM = "Unexpected term found";

    public static final int    ERR_INVALID_HANDLE = 2103;
    public static final String STR_INVALID_HANDLE = "Invalid stream handle";

    public static final int    ERR_CLASS_NOT_FOUND = 2104;
    public static final String STR_CLASS_NOT_FOUND = "Class not found";

    public static final int    ERR_CLASS_CAST = 2105;
    public static final String STR_CLASS_CAST = "Unexptected class found";

    public static final int    ERR_INSTANTIATION = 2106;
    public static final String STR_INSTANTIATION = "Instantiation error";

    public static final int    ERR_METHOD_NOT_FOUND = 2107;
    public static final String STR_METHOD_NOT_FOUND = "Method not found";

    public static final int    ERR_OBJECT_NOT_FOUND = 2108;
    public static final String STR_OBJECT_NOT_FOUND = "Object not found";

    private static Hashtable s_classHandleTbl;

    static
    {
        s_classHandleTbl  = new Hashtable(10);
    }

    public static final JIPAtom putObject(Object object)
    {
        String strHandle = "#" + object.hashCode();
        s_classHandleTbl.put(strHandle, object);
        return JIPAtom.create(strHandle);
    }

    public static final Object getObject(String strHandle)
    {
        if(s_classHandleTbl.containsKey(strHandle))
        {
            return s_classHandleTbl.get(strHandle);
        }
        else
        {
            return null;
        }
    }

    public static final void releaseObject(String strHandle)
    {
        if(s_classHandleTbl.containsKey(strHandle))
        {
            s_classHandleTbl.remove(strHandle);
        }
    }

    static final JIPTerm marshallOut(Object term)
    {
        if(term instanceof Number)
        {
            return JIPNumber.create(((Number)term).doubleValue());
        }
        else if(term instanceof String)
        {
            return JIPAtom.create((String)term);
        }
        else if(term instanceof Character)
        {
            return JIPNumber.create(((Character)term).charValue());
            //return JIPNumber.create(Character.getNumericValue(((Character)term).charValue()));
        }
        else if(term instanceof Boolean)
        {
            if(((Boolean)term).booleanValue())
                return JIPAtom.create("true");
            else
                return JIPAtom.create("false");
        }
        else if(term instanceof Void || term == null)
        {
            return JIPList.NIL;
        }
        else
        {
            // insert object in table
            return putObject(term);
        }
    }

    static final Object marshallIn(JIPTerm term)
    {
        if(term instanceof JIPNumber)
        {
            JIPNumber num = (JIPNumber) term;
            if(num.isInteger())
            {
                int nVal = (int)num.getDoubleValue();

                if(Math.abs(nVal) > Integer.MAX_VALUE)
                    return new Long(nVal);
                else
                    return new Integer(nVal);
            }
            else
            {
                return new Double(num.getDoubleValue());
            }
        }
        else if(term instanceof JIPAtom)
        {
            String strAtom = term.toString();

            if(strAtom.startsWith("#"))
            {
                Object obj = JIPxReflect.getObject(strAtom);
                if(obj != null)
                    return obj;
                else  // Object not found
                    throw new JIPRuntimeException(JIPxReflect.ERR_UNEXPECTED_TERM, JIPxReflect.STR_UNEXPECTED_TERM);
            }
            else if(strAtom.equals("true") || strAtom.equals("false"))
            {
                return new Boolean(strAtom);
            }
            else
            {
                return strAtom;
            }

        }
        else if(term instanceof JIPString)
        {
            return term.toString();
        }
        else
        {
            return term;
        }
    }

    static Class[] getParamsClass(JIPTerm className) throws NoSuchMethodException, ClassNotFoundException
    {
        if(className instanceof JIPAtom)
        {
            // get the class
            Class paramClass[] = new Class[0];

            // get the rigth method
            return paramClass;
        }
        else
        {
            // manage prototype
            Vector classVect = new Vector();
            JIPCons params = ((JIPFunctor)className).getParams();
            while(params != null && !params.isNIL())
            {
                String strClassName = ((JIPAtom)getTerm(params.getHead())).getName();
                Class paramClass;
                if(strClassName.equals("int"))
                {
                    paramClass = Integer.TYPE;
                }
                else if(strClassName.equals("char"))
                {
                    paramClass = Character.TYPE;
                }
                else if(strClassName.equals("byte"))
                {
                    paramClass = Byte.TYPE;
                }
                else if(strClassName.equals("boolean"))
                {
                    paramClass = Boolean.TYPE;
                }
                else if(strClassName.equals("float"))
                {
                    paramClass = Float.TYPE;
                }
                else if(strClassName.equals("double"))
                {
                    paramClass = Double.TYPE;
                }
                else if(strClassName.equals("long"))
                {
                    paramClass = Long.TYPE;
                }
                else if(strClassName.equals("short"))
                {
                    paramClass = Short.TYPE;
                }
                else
                {
                    paramClass = className.getClass().forName(strClassName);
                }

                classVect.addElement(paramClass);
                params = (JIPCons)getTerm(params.getTail());
            }

            Class paramClass[] = new Class[classVect.size()];
            classVect.copyInto(paramClass);

            // get the rigth constructor
            return paramClass;
        }
    }

    private static JIPTerm getTerm(JIPTerm term)
    {
        if(term instanceof JIPVariable)
        {
            if(((JIPVariable)term).isBounded())
                return ((JIPVariable)term).getValue();
        }

        return term;
    }
}

