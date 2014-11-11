package com.ugos.jiprolog.extensions.io;

import java.io.IOException;
import java.util.Enumeration;

import com.ugos.io.PushBackInputStream;

public class InputStreamInfo extends StreamInfo
{
    PushBackInputStream m_stream;
    Enumeration m_enum;

    public InputStreamInfo(String name, String handle)
    {
    	super(name, handle);
    	init();
    }

    public InputStreamInfo(String name)
	{
    	super(name);
    	init();
	}

    private void init()
    {
    	properties.setProperty("mode", "read");
		properties.setProperty("input", "");
		properties.setProperty("eof_action", "eof_code");
	}

    public int getLineNumber()
    {
    	return m_stream.getLineNumber();
    }

    public int getPosition()
    {
    	return m_stream.getRead();
    }

    public boolean isEOF() throws IOException
    {
    	m_stream.mark(1);

    	int i = m_stream.read();
    	if( i == -1 )
    		return true;

    	m_stream.pushback();

    	return false;
    }

}
