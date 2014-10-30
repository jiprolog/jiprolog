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

public final class CopyOfStreamProperty3 extends JIPXCall
{
	private static Hashtable<String, Hashtable<String, JIPTerm>> streamPropertyTbl = new Hashtable<String, Hashtable<String, JIPTerm>>();

	private Hashtable<String, JIPTerm> propsTbl;
	private Enumeration<JIPTerm> termEnum;
	private Enumeration<String> handleEnum;
	private String handle;

	private JIPAtom op;

//	xcall('com.ugos.jiprolog.extensions.io.StreamProperty2', [set, '#123', file_name(ugo)]).
    public final boolean unify(final JIPCons params, Hashtable<JIPVariable, JIPVariable> varsTbl)
    {
        // get first parameter
    	op = (JIPAtom)params.getNth(1).getValue();
    	if(op == null)
            throw new JIPParameterUnboundedException(1);

        JIPTerm handle = params.getNth(2);

        JIPTerm prop = params.getNth(3);

        JIPTerm prop1 = prop.getValue();

        if(op.getName().equals("set"))
        {
        	handle = handle.getValue();
            if(handle == null)
                throw new JIPParameterUnboundedException(2);

            if(!(handle instanceof JIPAtom))
                throw new JIPParameterTypeException(2, JIPParameterTypeException.ATOM);

        	if(prop1 == null)
                throw new JIPParameterUnboundedException(3);

        	propsTbl = streamPropertyTbl.get(((JIPAtom)handle).getName());
        	if(propsTbl == null)
        	{
        		propsTbl = new Hashtable<String, JIPTerm>();
        		streamPropertyTbl.put(((JIPAtom)handle).getName(), propsTbl);
        	}

        	if(prop1 instanceof JIPAtom)
        	{
        		propsTbl.put(((JIPAtom)prop1).getName(), JIPList.NIL);
        	}
        	else if(prop1 instanceof JIPFunctor)
        	{
        		String key = ((JIPFunctor)prop1).getDefinition();
        		propsTbl.put(key, prop1);
        	}
        	else
                throw new JIPParameterTypeException(3, JIPParameterTypeException.COMPOUND);

        	return true;
        }
        else if(op.getName().equals("remove"))
        {
        	handle = handle.getValue();
            if(handle == null)
                throw new JIPParameterUnboundedException(2);

            if(!(handle instanceof JIPAtom))
                throw new JIPParameterTypeException(2, JIPParameterTypeException.ATOM);

        	String streamName = ((JIPAtom)handle).getName();
        	if(streamName.equals("user_input") || streamName.equals("user_output") || streamName.equals("user_error"))
        	{
        		throw new JIPRuntimeException("Standard stream " + streamName + " cannot be closed");
        	}

        	streamPropertyTbl.remove(streamName);

        	return true;
        }
        else if(op.getName().equals("get"))
        {
        	if(handle.getValue() == null)
        	{
        		if(handleEnum == null)
        		{
        			handleEnum = streamPropertyTbl.keys();

	        		if(handleEnum.hasMoreElements())
	        		{
	        			this.handle  = handleEnum.nextElement();
	    	        	propsTbl = streamPropertyTbl.get(this.handle);
	    	        	if(propsTbl == null)
	    	        		return false;

	    	        	handle.unify(JIPAtom.create(this.handle), varsTbl);
	        		}
        		}
        		else if(termEnum != null && !termEnum.hasMoreElements())
        		{
        			if(handleEnum.hasMoreElements())
	        		{
	        			this.handle  = handleEnum.nextElement();
	    	        	propsTbl = streamPropertyTbl.get(this.handle);
	    	        	if(propsTbl == null)
	    	        		return false;

	    	        	termEnum = null;
	    	        	handle.unify(JIPAtom.create(this.handle), varsTbl);
	        		}
        			else
        			{
        				return false;
        			}
        		}
        	}
        	else
        	{
	            if(!(handle instanceof JIPAtom))
	                throw new JIPParameterTypeException(2, JIPParameterTypeException.ATOM);

	        	if(propsTbl == null)
		        {
		        	propsTbl = streamPropertyTbl.get(((JIPAtom)handle).getName());
		        	if(propsTbl == null)
		        		return false;
		        }
        	}

            if(prop1 == null)
            {
		        if(termEnum == null)
		        	termEnum = propsTbl.elements();

		        while(termEnum.hasMoreElements())
		        {
		        	JIPTerm term = termEnum.nextElement();

		        	if(prop.unifiable(term))
		        		return prop.unify(term, varsTbl);
		        }

		        return false;
            }
            else
            {
            	JIPTerm term;
            	if(prop1 instanceof JIPAtom)
            	{
            		term = propsTbl.get(((JIPAtom)prop1).getName());
            	}
            	else if(prop1 instanceof JIPFunctor)
            	{
            		String key = ((JIPFunctor)prop1).getDefinition();
            		term = propsTbl.get(key);
            	}
            	else
                    throw new JIPParameterTypeException(3, JIPParameterTypeException.COMPOUND);

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

