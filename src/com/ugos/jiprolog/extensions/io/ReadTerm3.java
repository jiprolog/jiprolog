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
import com.ugos.jiprolog.engine.JIPTermParser.TermEnumerator;

import java.io.*;
import java.util.*;

public final class ReadTerm3 extends JIPXCall
{
    private Enumeration m_termEnum;
    private boolean     m_bEOF;
    private int      m_streamHandle;

    private StreamInfo streamInfo;
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        if(m_bEOF)
            return false;

        int lineBegin;
        if(m_termEnum == null) // First time
        {
            // get first parameter
            JIPTerm input = params.getNth(1);

            // check if input is a variable
            if (input instanceof JIPVariable)
            {
                // try to extract the term
                if(!((JIPVariable)input).isBounded())
                {
                    throw new JIPInstantiationException(1);
                }
                else
                {
                    //extracts the term
                    input = ((JIPVariable)input).getValue();
                }
            }

                streamInfo = JIPio.getInputStreamInfo(input, false);
                m_streamHandle = streamInfo.getHandle();

//                String mode = streamInfo.getProperties().getProperty("mode");
//                if(!(mode.equals("mode(read)")))
//                	throw new JIPPermissionException("input", "stream", streamInfo.getAlias());
//                if(!streamInfo.getProperties().getProperty("type").equals("type(text)"))
//                	throw new JIPPermissionException("input", "binary_stream", streamInfo.getAlias());

                // Get the stream
                m_termEnum = JIPio.getTermEnumeration(m_streamHandle, getJIPEngine());
                if(m_termEnum == null)
                {
                	throw JIPExistenceException.createSourceSynkException(JIPNumber.create(m_streamHandle));
                }
        }

        boolean bUserStream;
        if(bUserStream = JIPEngine.USER_INPUT_HANDLE == m_streamHandle)
            getJIPEngine().notifyEvent(JIPEvent.ID_WAITFORUSERINPUT, getPredicate(), getQueryHandle());

        JIPFunctor singleton = null;
        JIPFunctor variable_names = null;
        JIPFunctor line_counts = null;

//        JIPFunctor variables = null;

        JIPList options = (JIPList)params.getNth(3).getValue();

        JIPFunctor sf = JIPFunctor.create("singletons", JIPCons.create(JIPVariable.create("Vars"), null));
        int pos = options.member(sf);

        if(pos > 0)
        	singleton = (JIPFunctor)options.getNth(pos);

        JIPFunctor vn = JIPFunctor.create("variable_names", JIPCons.create(JIPVariable.create("Vars"), null));
        pos = options.member(vn);

        if(pos > 0)
        	variable_names = (JIPFunctor)options.getNth(pos);

        JIPFunctor lc = JIPFunctor.create("line_counts", JIPCons.create(JIPVariable.create("Begin"), JIPCons.create(JIPVariable.create("End"), null)));
        pos = options.member(lc);

        if(pos > 0)
        	line_counts = (JIPFunctor)options.getNth(pos);

//        JIPFunctor va = JIPFunctor.create("variables", JIPCons.create(JIPVariable.create("Vars"), null));
//        pos = options.member(va);
//
//        if(pos > 0)
//        	variables = (JIPFunctor)options.getNth(pos);

        JIPTerm term;
        try
        {
//        	lineBegin = ((InputStreamInfo)streamInfo).getLineNumber();

            if(m_termEnum.hasMoreElements())
            {
                term = (JIPTerm)m_termEnum.nextElement();
                if(singleton != null)
                {
                	JIPList singletonVars = ((TermEnumerator)m_termEnum).getSingletonVariables();

                	if(singletonVars == null)
                		singletonVars = JIPList.NIL;

                	if(!singleton.getParams().getNth(1).unify(singletonVars, varsTbl))
                	{
                        if(bUserStream)
                            getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

                		return false;
                	}
                }

                if(variable_names != null)
                {
                	JIPVariable[] vars = term.getVariables();
                	JIPList varsName = null;

                	if(vars == null || vars.length == 0)
                	{
                		varsName = JIPList.NIL;
                	}
                	else
                	{
                    	for(JIPVariable var : vars)
                    	{
                    		if(!var.isAnonymous())
                    			varsName = JIPList.create(JIPFunctor.create("=", JIPCons.create(JIPAtom.create(var.getName()), JIPCons.create(var, null))), varsName);
                    	}
                	}

                	if( varsName == null)
                        varsName = JIPList.NIL;
                	else
						varsName = varsName.reverse();

                	if(!variable_names.getParams().getNth(1).unify(varsName, varsTbl))
                	{
                        if(bUserStream)
                            getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

                		return false;
                	}
                }

                if(line_counts != null)
                {
                	int lineEnd = ((InputStreamInfo)streamInfo).getLineNumber();

                	JIPFunctor lc1 = JIPFunctor.create("line_counts", JIPCons.create(JIPNumber.create(term.getLine()), JIPCons.create(JIPNumber.create(lineEnd), null)));

                	if(!line_counts.unify(lc1, varsTbl))
                	{
                        if(bUserStream)
                            getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

                		return false;
                	}
                }

//                if(variables != null)
//                {
//                	JIPVariable[] vars = term.getVariables();
//                	JIPList vars1 = null;
//
//                	if(vars == null || vars.length == 0)
//                	{
//                		vars1 = JIPList.NIL;
//                	}
//                	else
//                	{
//                    	for(JIPVariable var : vars)
//                    	{
//                    		vars1 = JIPList.create(var, vars1);
//                    	}
//                	}
//
//                	if(!variables.getParams().getNth(1).unify(vars1, varsTbl))
//                	{
//                        if(bUserStream)
//                            getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());
//
//                		return false;
//                	}
//
//                }

            }
            else
            {
                if(bUserStream)
                {
                    term = JIPCons.NIL;
                }
                else
                {
                    m_bEOF = true;
                    term = JIPAtom.create("end_of_file");
                    streamInfo.setEndOfStream("past");
                }
            }
        }
        catch(JIPRuntimeException ex)
        {
            if(bUserStream)
                getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

            throw ex;
        }

        if(bUserStream)
            getJIPEngine().notifyEvent(JIPEvent.ID_USERINPUTDONE, getPredicate(), getQueryHandle());

        // true if the term read unify with the second paramater
        return params.getNth(2).unify(term, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

