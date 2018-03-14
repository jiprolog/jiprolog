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

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.ugos.io.*;
import com.ugos.jiprolog.engine.*;

public final class JIPio
{
    public static final int    ERR_IOEXCEPTION  = 2000;

    private static InputStreamInfo user_input = new InputStreamInfo("user_input", JIPEngine.USER_INPUT_HANDLE, "read", "reset");
    private static OutputStreamInfo user_output = new OutputStreamInfo("user_output", JIPEngine.USER_OUTPUT_HANDLE, "append");
    private static OutputStreamInfo user_error = new OutputStreamInfo("user_error", JIPEngine.USER_ERROR_HANDLE, "append");

    public static Hashtable<Integer, InputStreamInfo> itable = new Hashtable<Integer, InputStreamInfo>();
    public static Hashtable<Integer, OutputStreamInfo> otable = new Hashtable<Integer, OutputStreamInfo>();

    public static Hashtable<String, StreamInfo> iotable = new Hashtable<String, StreamInfo>();


    static
    {
    	user_input.setAlias("user_input");
    	user_output.setAlias("user_output");
    	user_error.setAlias("user_error");

    	itable.put(user_input.getHandle(), user_input);
    	otable.put(user_output.getHandle(), user_output);
    	otable.put(user_error.getHandle(), user_error);

    	iotable.put(user_input.getAlias(), user_input);
    	iotable.put(user_output.getAlias(), user_output);
    	iotable.put(user_error.getAlias(), user_error);
    }

    public static void init(JIPEngine engine)
    {
    	try
    	{
    		openInputStream("user_input", JIPEngine.USER_INPUT_HANDLE, engine);
			openOutputStream("user_output", JIPEngine.USER_OUTPUT_HANDLE, false, engine);
			openOutputStream("user_error", JIPEngine.USER_ERROR_HANDLE, false, engine);
		}
    	catch (IOException e)
    	{
			e.printStackTrace();
		}
    }

    public static Enumeration<Integer> getInputHandles()
    {
    	return itable.keys();
    }

    public static Enumeration<Integer> getOutputHandles()
    {
    	return otable.keys();
    }

    private static final int put(final OutputStreamInfo obj)
    {
        // put the new enumeration in the table
        otable.put(obj.getHandle(), obj);
        iotable.put(obj.getAlias(), obj);

        return obj.getHandle();
    }

    private static final int put(final InputStreamInfo obj)
    {
        // put the new enumeration in the table
        itable.put(obj.getHandle(), obj);
        iotable.put(obj.getAlias(), obj);

        return obj.getHandle();
    }


    static final InputStreamInfo getInput(final int handle)
    {
        return itable.get(handle);
    }

    static final OutputStreamInfo getOutput(final int handle)
    {
        return otable.get(handle);
    }

    private static final void remove(final int handle)
    {
        // get iotable (opened file)
    	if(itable.containsKey(handle))
    		itable.remove(handle);

    	if(otable.containsKey(handle))
    		otable.remove(handle);

    }

    @SuppressWarnings("resource")
	public static final int openInputStream(String strPath, final int handle, final JIPEngine engine) throws IOException
    {
        InputStream reader;

        if(strPath.equals("user_input"))
        {
            reader = engine.getUserInputStream();
            InputStreamInfo sinfo = itable.get(JIPEngine.USER_INPUT_HANDLE);

            reader = new PushbackLineNumberInputStream(reader);

            sinfo.m_stream = (PushbackLineNumberInputStream)reader;

            return sinfo.getHandle();
        }
        else
        {
            // try path as URL
            try
            {
                URL url = new URL(strPath);
                reader = url.openStream();
            }
            catch(IOException ex)
            {
                strPath = strPath.replace((char)92, File.separatorChar);
                strPath = strPath.replace('/', File.separatorChar);

                File ffile = new File(strPath);
                if(!ffile.isAbsolute())
                {
                    ffile = new File(engine.getSearchPath() + File.separator + strPath);
                    strPath = ffile.getAbsolutePath();
                }
                // try as normal path
                reader = new FileInputStream(strPath);
            }

	        reader = new PushbackLineNumberInputStream(reader);

	        InputStreamInfo sinfo =
	        		new InputStreamInfo(strPath,
	        							handle,
	        							"read",
	        						    "eof_code");

	        sinfo.m_stream = (PushbackLineNumberInputStream)reader;

//	        int n = sinfo.m_stream.read();
//	        if(n == -1)
//	        	sinfo.setEndOfStream("at");
//	        else
//	        	sinfo.m_stream.pushback();

	        return put(sinfo);
        }
    }

    public static final int openOutputStream(String strPath, final int handle, boolean bAppend, final JIPEngine engine) throws IOException
    {
        OutputStream writer;

        if(strPath.equals("user_output") || strPath.equals("user_error"))
        {
            writer = engine.getUserOutputStream();

            OutputStreamInfo sinfo = otable.get(JIPEngine.USER_OUTPUT_HANDLE);

            sinfo.m_stream = writer;

            return sinfo.getHandle();
        }
        else
        {
            // try path as URL
            try
            {
                URL url = new URL(strPath);
                URLConnection urlConn = url.openConnection();
                writer = urlConn.getOutputStream();
            }
            catch(IOException ex)
            {
                File ffile = new File(strPath);
                if(!ffile.isAbsolute())
                {
                    ffile = new File(engine.getSearchPath() + File.separator + strPath);
                    strPath = ffile.getAbsolutePath();
                }

                // try as normal path
                writer = new FileOutputStream(strPath, bAppend);
            }

	        OutputStreamInfo sinfo =
	        		new OutputStreamInfo(strPath,
	        							 handle,
	        							 bAppend ? "append" : "write");

	        sinfo.m_stream = writer;

	        return put(sinfo);
        }
    }

    public final static Enumeration getTermEnumeration(final int handle, final JIPEngine engine )
    {
         // get term parser
        JIPTermParser termParser = engine.getTermParser();

        if(handle == JIPEngine.USER_INPUT_HANDLE)
        {
            // get the enumeration of terms in the file
            return termParser.parseStream(new PushbackLineNumberInputStream(engine.getUserInputStream()), "user_input");
        }

        InputStreamInfo sinfo = getInput(handle);
        if(sinfo != null)
        {
            if(sinfo.m_enum == null)
                // get the enumeration of terms in the file
                sinfo.m_enum = termParser.parseStream(sinfo.m_stream, sinfo.getName());

            return sinfo.m_enum;
        }
        else
            return null;
    }

    public final static PushbackLineNumberInputStream getInputStream(final int handle, final JIPEngine engine)
    {
        if(handle == JIPEngine.USER_INPUT_HANDLE)
        {
            return new PushbackLineNumberInputStream(engine.getUserInputStream());
        }
        else if(handle == JIPEngine.USER_OUTPUT_HANDLE || handle == JIPEngine.USER_ERROR_HANDLE)
        {
        	throw new JIPPermissionException("input", "stream", JIPNumber.create(handle));
        }

        InputStreamInfo sinfo = getInput(handle);
        if(sinfo != null)
            return (PushbackLineNumberInputStream)sinfo.m_stream;
        else
            return null;
    }

    public static OutputStream getOutputStream(final int handle, final JIPEngine engine)
    {
        if(handle == JIPEngine.USER_OUTPUT_HANDLE || handle == JIPEngine.USER_ERROR_HANDLE)
        {
            return engine.getUserOutputStream();
        }
        else if(handle == JIPEngine.USER_INPUT_HANDLE)
        {
        	throw new JIPPermissionException("output", "stream", JIPNumber.create(handle));
        }

        OutputStreamInfo sinfo = getOutput(handle);

        if(sinfo != null)
            return (OutputStream)sinfo.m_stream;
        else
            return null;
    }

    public static void setStreamAlias(StreamInfo sinfo, String alias)
    {
//    	System.out.println("set alias " + alias);

        iotable.remove(sinfo.getAlias());
		sinfo.getProperties().setProperty("alias", String.format("alias(%s)",alias));
        iotable.put(alias, sinfo);
    }

    public static void setStreamType(StreamInfo sinfo, String type)
    {
     	sinfo.getProperties().setProperty("type", String.format("type(%s)",type));
     	sinfo.setBinary(type.equals("binary"));
    }

    public static StreamInfo getInputStreamInfo(JIPTerm streamOrAlias, boolean binary) throws JIPExistenceException, JIPDomainException, JIPInstantiationException
    {
    	StreamInfo sinfo = getStreamInfo(streamOrAlias);
    	if(sinfo instanceof OutputStreamInfo)
    		throw new JIPPermissionException("input", "stream", sinfo.getAlias());

    	if(binary && !sinfo.isBinary())
    		throw new JIPPermissionException("input", "text_stream", sinfo.getAlias());
    	else if(sinfo.isBinary() && !binary)
    		throw new JIPPermissionException("input", "binary_stream", sinfo.getAlias());


    	return sinfo;
    }

    public static StreamInfo getOutputStreamInfo(JIPTerm streamOrAlias, boolean binary) throws JIPExistenceException, JIPDomainException, JIPInstantiationException
    {
    	StreamInfo sinfo = getStreamInfo(streamOrAlias);

    	if(sinfo instanceof InputStreamInfo)
    		throw new JIPPermissionException("output", "stream", sinfo.getAlias());

    	if(binary && !sinfo.isBinary())
    		throw new JIPPermissionException("output", "text_stream", sinfo.getAlias());
    	else if(sinfo.isBinary() && !binary)
    		throw new JIPPermissionException("output", "binary_stream", sinfo.getAlias());

    	return sinfo;

    }

    public static StreamInfo getStreamInfo(JIPTerm streamOrAlias) throws JIPExistenceException, JIPDomainException, JIPInstantiationException
    {
    	StreamInfo sinfo = null;

    	streamOrAlias = streamOrAlias.getValue();
    	if(streamOrAlias instanceof JIPNumber)
    	{
    		sinfo = getStreamInfo((int)((JIPNumber)streamOrAlias).getDoubleValue());
    	}
    	else if(streamOrAlias instanceof JIPAtom)
    	{
    		sinfo = getStreamInfo(getStreamHandle(((JIPAtom)streamOrAlias).getName()));
    	}
    	else if(streamOrAlias == null)
    	{
    		throw new JIPInstantiationException(1);
    	}
    	else
    		throw new JIPDomainException("stream_or_alias", streamOrAlias);

    	if(sinfo == null)
    		throw JIPExistenceException.createStreamException(streamOrAlias);

    	return sinfo;
    }

    private static StreamInfo getStreamInfo(final int handle)
    {
    	StreamInfo sinfo = null;

    	if(itable.containsKey(handle))
    	{
    		sinfo = getInput(handle);
    	}
    	else if(otable.containsKey(handle))
    	{
    		sinfo = getOutput(handle);
    	}

    	return sinfo;
    }

    public static String getStreamName(final int handle)
    {
    	StreamInfo sinfo = null;

    	if(itable.containsKey(handle))
    	{
    		sinfo = getInput(handle);
    	}
    	else if(otable.containsKey(handle))
    	{
    		sinfo = getOutput(handle);
    	}


        if(sinfo != null)
            return sinfo.getName();
        else
            return null;
    }

    public static int getStreamHandle(final String alias)
    {
//    	for(StreamInfo sinfo : iotable.values())
//    	{
//    		if(alias.equals(sinfo.getAlias()))
//    			return sinfo.getHandle();
//    	}
    	StreamInfo sinfo;

//    	System.out.println(alias);
//    	System.out.println(iotable);

    	if(iotable.containsKey(alias))
    	{
    		sinfo = iotable.get(alias);
    		return sinfo.getHandle();
    	}

        return 0;
    }

    public static void closeInputStream(final int handle) throws IOException
    {
        if(handle == JIPEngine.USER_INPUT_HANDLE || handle == JIPEngine.USER_OUTPUT_HANDLE || handle == JIPEngine.USER_ERROR_HANDLE)
        {
            return;//throw new JIPRuntimeException(ERR_USER_STREAM, STR_JIPEngine.USER_STREAM);
        }

        InputStreamInfo sinfo = getInput(handle);
        if(sinfo != null)
        {
            ((InputStream)sinfo.m_stream).close();
            remove(handle);
        }
    }

    public static void closeOutputStream(final int handle) throws IOException
    {
        if(handle == JIPEngine.USER_INPUT_HANDLE || handle == JIPEngine.USER_OUTPUT_HANDLE || handle == JIPEngine.USER_ERROR_HANDLE)
        {
            return;//throw new JIPRuntimeException(ERR_USER_STREAM, STR_USER_STREAM);
        }

        OutputStreamInfo sinfo = getOutput(handle);
        if(sinfo != null)
        {
            ((OutputStream)sinfo.m_stream).close();
            remove(handle);
        }
    }
    
    public static String resolvePath(String path)
    {
    	Map<String, String> envMap = System.getenv();
    	
    	String pattern = "(\\%|\\$)([A-Za-z0-9|_]+)(\\%)?"; 
    	
    	Pattern expr = Pattern.compile(pattern);
    	Matcher matcher = expr.matcher(path);
    	while (matcher.find()) {
    	    String envValue = envMap.get(matcher.group(2).toUpperCase());
    	    if (envValue == null) {
    	        envValue = "";
    	    } else {
    	        envValue = envValue.replace("\\", "\\\\");
    	    }
    	    Pattern subexpr = Pattern.compile(Pattern.quote(matcher.group(0)));
    	    path = subexpr.matcher(path).replaceAll(envValue);
    	}
    	
    	return path;
    	
    }


}

