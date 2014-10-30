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

    public static final int    ERR_INVALID_HANDLE = 2001;
    public static final String STR_INVALID_HANDLE = "Invalid stream handle";
    /*
    public static final int    ERR_FILE_NOT_FOUND = 2002;
    public static final String STR_FILE_NOT_FOUND = "File not found";
     */
    public static final int    ERR_FILE_NOT_DELETED = 2003;
    public static final String STR_FILE_NOT_DELETED = "Unable to delete the file/directory";

    public static final int    ERR_FILE_NOT_RENAMED = 2004;
    public static final String STR_FILE_NOT_RENAMED = "Unable to rename the file/directory";

    public static final int    ERR_DIRECTORY_NOT_CREATED = 2005;
    public static final String STR_DIRECTORY_NOT_CREATED = "Unable to create the directory";

    public static final int    ERR_USER_STREAM = 2006;
    public static final String STR_USER_STREAM = "Operation not permitted on the given stream handle";

    private static StreamInfo user_input = new StreamInfo("user_input", "user_input");
    private static StreamInfo user_output = new StreamInfo("user_output", "user_output");
    private static StreamInfo user_error = new StreamInfo("user_error", "user_error");

    public static Hashtable<String, StreamInfo> iotable = new Hashtable<String, JIPio.StreamInfo>();

    static
    {
    	iotable.put("user_input", user_input);
    	iotable.put("user_output", user_output);
    	iotable.put("user_error", user_error);
    }
    private static final String put(final StreamInfo obj, final JIPEngine engine)
    {
        // put the new enumeration in the table
        iotable.put(obj.m_handle.toString(), obj);

        return obj.m_handle.toString();
    }

    static final StreamInfo get(final String handle, final JIPEngine engine)
    {
        return (StreamInfo)iotable.get(handle);
    }


    private static final void remove(final String handle, final JIPEngine engine)
    {
        // get iotable (opened file)
        iotable.remove(handle);
    }

    public static final String openInputStream(String strPath, final String strHandle, final JIPEngine engine) throws IOException
    {
        InputStream reader;

        if(strPath.equals("user_input"))
        {
            reader = engine.getUserInputStream();
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
        }

        reader = new PushBackInputStream(reader);

        StreamInfo sinfo = new StreamInfo();
        sinfo.m_strName = strPath;
        sinfo.m_stream = reader;
        //sinfo.m_pointer = termEnum;
        if(strHandle != null)
            sinfo.m_handle  = strHandle;
        else
            sinfo.m_handle = "#" + sinfo.hashCode();

        return put(sinfo, engine);
    }

    public static final String openOutputStream(String strPath, final String strHandle, boolean bAppend, final JIPEngine engine) throws IOException
    {
        OutputStream writer;

        if(strPath.equals("user_output") || strPath.equals("user_error"))
        {
            writer = engine.getUserOutputStream();
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
        }

        StreamInfo sinfo = new StreamInfo();
        sinfo.m_strName = strPath;
        sinfo.m_stream = writer;
        //sinfo.m_pointer = writer;

        if(strHandle != null)
            sinfo.m_handle  = strHandle;
        else
            sinfo.m_handle = sinfo.m_handle = "#" + sinfo.hashCode();


        return put(sinfo, engine);
    }

    public final static Enumeration getTermEnumeration(final String strHandle , final JIPEngine engine)
    {
         // get term parser
        JIPTermParser termParser = engine.getTermParser();

        if(strHandle.equals("user_input"))
        {
            // get the enumeration of terms in the file
            return termParser.parseStream(engine.getUserInputStream(), "user_input");
        }

        StreamInfo sinfo = get(strHandle, engine);
        if(sinfo != null)
        {
            if(sinfo.m_enum == null)
                // get the enumeration of terms in the file
                sinfo.m_enum = termParser.parseStream((InputStream)sinfo.m_stream, sinfo.m_strName);

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
            throw new JIPRuntimeException(ERR_USER_STREAM, STR_USER_STREAM);
        }

        StreamInfo sinfo = get(strHandle, engine);
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
            throw new JIPRuntimeException(ERR_USER_STREAM, STR_USER_STREAM);
        }

        StreamInfo sinfo = get(strHandle, engine);

        if(sinfo != null)
            return (OutputStream)sinfo.m_stream;
        else
            return null;
    }

    public static String getStreamName(final String strHandle, final JIPEngine engine)
    {
        StreamInfo sinfo = get(strHandle, engine);
        if(sinfo != null)
            return sinfo.m_strName;
        else
            return null;
    }

    public static void closeInputStream(final String strHandle, final JIPEngine engine) throws IOException
    {
        if(strHandle.equals("user_input") || strHandle.equals("user_output") || strHandle.equals("user_error"))
        {
            throw new JIPRuntimeException(ERR_USER_STREAM, STR_USER_STREAM);
        }

        StreamInfo sinfo = get(strHandle, engine);
        if(sinfo != null)
        {
            ((InputStream)sinfo.m_stream).close();
            remove(strHandle, engine);
        }
    }

    public static void closeOutputStream(final String strHandle, final JIPEngine engine) throws IOException
    {
    	if(strHandle.equals("user_input") || strHandle.equals("user_output") || strHandle.equals("user_error"))
        {
            throw new JIPRuntimeException(ERR_USER_STREAM, STR_USER_STREAM);
        }

        StreamInfo sinfo = get(strHandle, engine);
        if(sinfo != null)
        {
            ((OutputStream)sinfo.m_stream).close();
            remove(strHandle, engine);
        }
    }

    public static class StreamInfo
    {
    	StreamInfo()
    	{

    	}

    	StreamInfo(String name, String handle)
    	{
    		m_strName = name;
    		m_handle = handle;
    	}

        String m_strName;
        Object m_stream;
        //Object m_pointer;
        String m_handle;
        Enumeration m_enum;
    }
}

