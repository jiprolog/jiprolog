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

public final class StreamProperty3 extends JIPXCall
{
	private Enumeration<Object> termEnum;

	private JIPAtom op;

//	xcall('com.ugos.jiprolog.extensions.io.StreamProperty2', [set, '#123', file_name(ugo)]).
    public final boolean unify(final JIPCons params, Hashtable<JIPVariable, JIPVariable> varsTbl)
    {
        // get first parameter
    	op = (JIPAtom)params.getNth(1).getValue();
    	if(op == null)
            throw new JIPInstantiationException(1);

        JIPTerm handle = params.getNth(2).getValue();
        if(handle == null)
            throw new JIPInstantiationException(2);

    	int nhandle = (int)((JIPNumber)handle).getDoubleValue();

    	StreamInfo streamInfo = JIPio.getStreamInfo(nhandle);
    	if(streamInfo == null)
    		return false;

        if(!(handle instanceof JIPAtom))
            throw new JIPTypeException(JIPTypeException.ATOM, handle);

        JIPTerm prop = params.getNth(3);

        JIPTerm prop1 = prop.getValue();

        if(op.getName().equals("set"))
        {
        	if(prop1 == null)
                throw new JIPInstantiationException(3);


        	if(prop1 instanceof JIPAtom)
        	{
        		streamInfo.getProperties().setProperty(((JIPAtom)prop1).getName(),"");
//            	System.out.println("set key " + ((JIPAtom)prop1).getName());

        	}
        	else if(prop1 instanceof JIPFunctor)
        	{
        		String key = ((JIPFunctor)prop1).getName();
//        		System.out.println("set key " + key);
//            	System.out.println("sterm " + prop1.toString());

        		streamInfo.getProperties().setProperty(key, prop1.toString());
        	}
        	else
                throw new JIPTypeException(JIPTypeException.COMPOUND, prop1);

        	return true;
        }
//        else if(op.getName().equals("remove"))
//        {
//        	String streamName = ((JIPAtom)handle).getName();
//        	if(streamName.equals("user_input") || streamName.equals("user_output") || streamName.equals("user_error"))
//        	{
//        		throw new JIPRuntimeException("Standard stream " + streamName + " cannot be closed");
//        	}
//
//        	streamPropertyTbl.remove(streamName);
//
//        	return true;
//        }
        else if(op.getName().equals("get"))
        {

        	if(prop1 == null)
            {
		        if(termEnum == null)
		        	termEnum = streamInfo.getProperties().keys();

		        while(termEnum.hasMoreElements())
		        {
		        	String key = (String)termEnum.nextElement();
		        	String sterm = streamInfo.getProperties().getProperty(key);

//		        	System.out.println("key " + key);
//		        	System.out.println("sterm " + sterm);

		        	JIPTerm term = getJIPEngine().getTermParser().parseTerm(sterm);
		        	if(prop.unifiable(term))
		        		return prop.unify(term, varsTbl);
		        }

		        return false;
            }
            else
            {
            	String sterm;

            	if(prop1 instanceof JIPAtom)
            	{
//            		System.out.println("key " + ((JIPAtom)prop1).getName());
            		sterm = streamInfo.getProperties().getProperty(((JIPAtom)prop1).getName());
            	}
            	else if(prop1 instanceof JIPFunctor)
            	{
            		String key = ((JIPFunctor)prop1).getName();
            		sterm = streamInfo.getProperties().getProperty(key);

//    	        	System.out.println("key " + key);

            	}
            	else
                    throw new JIPTypeException(JIPTypeException.COMPOUND, prop1);


            	if(sterm == null)
            	{
            		return false;
            	}

//	        	System.out.println("sterm " + sterm);

	        	JIPTerm term = getJIPEngine().getTermParser().parseTerm(sterm);
        		return prop.unify(term, varsTbl);
            }
        }
        else
        {
        	throw new JIPRuntimeException("Invalid stream operation " + op);
        }
    }

    public boolean hasMoreChoicePoints()
    {
        return termEnum != null && termEnum.hasMoreElements();
    }
}

