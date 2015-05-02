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
import java.io.*;
import java.util.*;

import com.ugos.io.PushbackLineNumberInputStream;

/**
 * JIPTermParser parses a prolog stream and create an enumeration of prologprolog terms (atom, functor, list, etc.)
 * @version 3.0
 * @author Ugo Chirico 200w<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPEngine#getTermParser
 */
public class JIPTermParser
{
    private OperatorManager m_opManager;
    private String m_encoding;
    private JIPEngine m_engine;
    private Hashtable<String, Variable> m_singletonVars;

    JIPTermParser(OperatorManager opManager, JIPEngine engine, String encoding)
    {
        m_opManager = opManager;
        m_encoding = encoding;
        m_engine = engine;
    }

    /** Returns an enumeration of terms contained in the specified input stream.
     * @param ins the input stream to parse.
     * @param streamName tha name of the stream (i.e. the name of the associated file)
     */
    public final Enumeration<JIPTerm> parseStream(final PushbackLineNumberInputStream ins, final String streamName)
    {
        return new TermEnumerator(ins, m_opManager, streamName);
    }

    /** Returns an enumeration of terms contained in the specified input stream.
     * @param ins the input stream to parse.
     * @param streamName tha name of the stream (i.e. the name of the associated file)
     */
    public final Enumeration<JIPTerm> parseStream(final PushbackLineNumberInputStream ins, final String streamName, final String encoding) throws UnsupportedEncodingException
    {
        return new TermEnumerator(ins, m_opManager, streamName);
    }

    /** Parses the term passed in the parameter and returns a JIPTerm object that wraps the corresponding prolog term.<br>
     * If the string passed in the parameter doesn't contains a valid prolog term it raises a JIPSyntaxErrorException.
     * @param strTerm Term to be parsed. If the term doesn't end with a "dot" one is appended to it.
     * @return a new JIPTerm object.
     * @exception com.ugos.jiprolog.engine.JIPSyntaxErrorException
     */
    public final JIPTerm parseTerm(String strTerm) throws JIPSyntaxErrorException
    {
        try
        {
        	final byte[] btTerm = strTerm.getBytes(getEncoding());
            final ByteArrayInputStream is = new ByteArrayInputStream(btTerm);
            PrologParser parser = new PrologParser(new ParserReader(new PushbackLineNumberInputStream(is)), m_opManager, m_engine, "user");

            m_singletonVars = parser.getSingletonVariables();

            final PrologObject term = parser.parseNext();

            return JIPTerm.getJIPTerm(term);
        }
        catch(UnsupportedEncodingException ex)
        {
            throw new JIPRuntimeException(ex.getMessage());
        }
    }

    public class TermEnumerator implements Enumeration<JIPTerm>, StreamPosition
    {
        private PrologParser m_parser;

        private JIPTerm m_nextTerm = null;

        TermEnumerator(final PushbackLineNumberInputStream ins, OperatorManager opManager, final String streamName)
        {
            m_parser = new PrologParser(new ParserReader(ins), opManager, m_engine, streamName);
        }

        private JIPTerm parseNextTerm() throws JIPSyntaxErrorException
        {
            final PrologObject term = m_parser.parseNext();
            if(term != null)
                return JIPTerm.getJIPTerm(term);
            else
                return null;
        }

        public boolean hasMoreElements() throws JIPSyntaxErrorException
        {
            if(m_nextTerm == null)
                m_nextTerm = parseNextTerm();

            return m_nextTerm != null;
        }

        public JIPTerm nextElement() throws JIPSyntaxErrorException
        {
            if(hasMoreElements())
            {
                JIPTerm aux = m_nextTerm;
                m_nextTerm = null;
                return aux;
            }
            else
                throw new NoSuchElementException();
        }

        public int getLineNumber()
        {
        	return m_parser.getLineNumber();
        }

        public JIPList getSingletonVariables()
        {
        	Hashtable<String, Variable> svar = m_parser.getSingletonVariables();

        	JIPList singletonVars = null;

//        	Hashtable<String, JIPVariable> sjvar = new Hashtable<String, JIPVariable>();

        	for(String key : svar.keySet())
        	{
        		Variable var = svar.get(key);
        		if(!var.isAnonymous())
        			singletonVars = JIPList.create(JIPFunctor.create("=", JIPCons.create(JIPAtom.create(var.getName()), JIPCons.create(new JIPVariable(var), null))), singletonVars);

//        		sjvar.put(key, new JIPVariable(var));
        	}
        	if(singletonVars == null)
				return JIPList.NIL;
        	else
				return singletonVars;//.reverse();
        }
    }

	/**
	 * @return the charset encoding
	 */
	public String getEncoding() {
		return m_encoding;
	}

	/**
	 * @param encoding the charset encoding to set
	 */
	public void setEncoding(String encoding) {
		this.m_encoding = encoding;
	}
}

