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

import com.ugos.io.*;
import com.ugos.jiprolog.engine.*;

public final class JIPio
{
    public static final int    ERR_IOEXCEPTION  = 2000;
//
//    public static final int    ERR_INVALID_HANDLE = 2001;
//    public static final String STR_INVALID_HANDLE = "Invalid stream handle";
    /*
    public static final int    ERR_FILE_NOT_FOUND = 2002;
    public static final String STR_FILE_NOT_FOUND = "File not found";
     */
//    public static final int    ERR_FILE_NOT_DELETED = 2003;
//    public static final String STR_FILE_NOT_DELETED = "Unable to delete the file/directory";
//
//    public static final int    ERR_FILE_NOT_RENAMED = 2004;
//    public static final String STR_FILE_NOT_RENAMED = "Unable to rename the file/directory";
//
//    public static final int    ERR_DIRECTORY_NOT_CREATED = 2005;
//    public static final String STR_DIRECTORY_NOT_CREATED = "Unable to create the directory";
//
//    public static final int    ERR_USER_STREAM = 2006;
//    public static final String STR_USER_STREAM = "Operation not permitted on the given stream handle";

    private static InputStreamInfo user_input = new InputStreamInfo("user_input", "user_input", "read", "reset");
    private static OutputStreamInfo user_output = new OutputStreamInfo("user_output", "user_output", "append", "reset");
    private static OutputStreamInfo user_error = new OutputStreamInfo("user_error", "user_error", "append", "reset");

    public static Hashtable<String, InputStreamInfo> itable = new Hashtable<String, InputStreamInfo>();
    public static Hashtable<String, OutputStreamInfo> otable = new Hashtable<String, OutputStreamInfo>();


    static
    {
    	itable.put("user_input", user_input);
    	otable.put("user_output", user_output);
    	otable.put("user_error", user_error);
    }

    public static void init(JIPEngine engine)
    {
    	try {
    		openInputStream("user_input", "user_input", engine);
			openOutputStream("user_output", "user_output", false, engine);
			openOutputStream("user_error", "user_error", false, engine);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }

    public static Enumeration<String> getInputHandles()
    {
    	return itable.keys();
    }

    public static Enumeration<String> getOutputHandles()
    {
    	return otable.keys();
    }

    private static final String put(final OutputStreamInfo obj)
    {
        // put the new enumeration in the table
        otable.put(obj.getHandle(), obj);

        return obj.getHandle();
    }

    private static final String put(final InputStreamInfo obj)
    {
        // put the new enumeration in the table
        itable.put(obj.getHandle(), obj);

        return obj.getHandle();
    }


    static final InputStreamInfo getInput(final String handle)
    {
        return itable.get(handle);
    }

    static final OutputStreamInfo getOutput(final String handle)
    {
        return otable.get(handle);
    }

    private static final void remove(final String handle)
    {
        // get iotable (opened file)
    	if(itable.containsKey(handle))
    		itable.remove(handle);

    	if(otable.containsKey(handle))
    		otable.remove(handle);

    }

    public static final String openInputStream(String strPath, final String strHandle, final JIPEngine engine) throws IOException
    {
        InputStream reader;

        if(strPath.equals("user_input"))
        {
            reader = engine.getUserInputStream();
            InputStreamInfo sinfo = itable.get("user_input");

            reader = new PushBackInputStream(reader);

            sinfo.m_stream = (PushBackInputStream)reader;

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

	        reader = new PushBackInputStream(reader);

	        InputStreamInfo sinfo =
	        		new InputStreamInfo(strPath,
	        							strHandle != null ? strHandle : "#" + strPath.hashCode(),
	        							"read",
	        						    "eof_code");

	        sinfo.m_stream = (PushBackInputStream)reader;

//	        int n = sinfo.m_stream.read();
//	        if(n == -1)
//	        	sinfo.setEndOfStream("at");
//	        else
//	        	sinfo.m_stream.pushback();

	        return put(sinfo);
        }
    }

    public static final String openOutputStream(String strPath, final String strHandle, boolean bAppend, final JIPEngine engine) throws IOException
    {
        OutputStream writer;

        if(strPath.equals("user_output") || strPath.equals("user_error"))
        {
            writer = engine.getUserOutputStream();

            OutputStreamInfo sinfo = otable.get(strPath);

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

	        OutputStreamInfo sinfo = new OutputStreamInfo(strPath);
	        sinfo.m_stream = writer;
	        //sinfo.m_pointer = writer;

	        if(strHandle != null)
	            sinfo.setHandle(strHandle);
	        else
	            sinfo.setHandle("#" + sinfo.hashCode());


	        return put(sinfo);
        }
    }

    public final static Enumeration getTermEnumeration(final String strHandle, final JIPEngine engine )
    {
         // get term parser
        JIPTermParser termParser = engine.getTermParser();

        if(strHandle.equals("user_input"))
        {
            // get the enumeration of terms in the file
            return termParser.parseStream(engine.getUserInputStream(), "user_input");
        }

        InputStreamInfo sinfo = getInput(strHandle);
        if(sinfo != null)
        {
            if(sinfo.m_enum == null)
                // get the enumeration of terms in the file
                sinfo.m_enum = termParser.parseStream((InputStream)sinfo.m_stream, sinfo.getName());

            return sinfo.m_enum;
        }
        else
            return null;
    }

    public final static PushBackInputStream getInputStream(final String strHandle, final JIPEngine engine)
    {
        if(strHandle.equals("user_input"))
        {
            return new PushBackInputStream(engine.getUserInputStream());
        }
        else if(strHandle.equals("user_output") || strHandle.equals("user_error"))
        {
        	throw new JIPPermissionException("input", "stream", JIPAtom.create(strHandle));
        }

        InputStreamInfo sinfo = getInput(strHandle);
        if(sinfo != null)
            return (PushBackInputStream)sinfo.m_stream;
        else
            return null;
    }

    public static OutputStream getOutputStream(final String strHandle, final JIPEngine engine)
    {
        if(strHandle.equals("user_output") || strHandle.equals("user_error"))
        {
            return engine.getUserOutputStream();
        }
        else if(strHandle.equals("user_input"))
        {
        	throw new JIPPermissionException("output", "stream", JIPAtom.create(strHandle));
        }

        OutputStreamInfo sinfo = getOutput(strHandle);

        if(sinfo != null)
            return (OutputStream)sinfo.m_stream;
        else
            return null;
    }

    public static StreamInfo getStreamInfo(final String handle)
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

    public static String getStreamName(final String handle)
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

    public static void closeInputStream(final String strHandle) throws IOException
    {
        if(strHandle.equals("user_input") || strHandle.equals("user_output") || strHandle.equals("user_error"))
        {
            return;//throw new JIPRuntimeException(ERR_USER_STREAM, STR_USER_STREAM);
        }

        InputStreamInfo sinfo = getInput(strHandle);
        if(sinfo != null)
        {
            ((InputStream)sinfo.m_stream).close();
            remove(strHandle);
        }
    }

    public static void closeOutputStream(final String strHandle) throws IOException
    {
    	if(strHandle.equals("user_input") || strHandle.equals("user_output") || strHandle.equals("user_error"))
        {
            return;//throw new JIPRuntimeException(ERR_USER_STREAM, STR_USER_STREAM);
        }

        OutputStreamInfo sinfo = getOutput(strHandle);
        if(sinfo != null)
        {
            ((OutputStream)sinfo.m_stream).close();
            remove(strHandle);
        }
    }


}

