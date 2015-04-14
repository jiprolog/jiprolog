package com.ugos.jiprolog.extensions.io;

import java.io.IOException;
import java.util.Enumeration;

import com.ugos.io.PushBackInputStream;

public class InputStreamInfo extends StreamInfo
{
    PushBackInputStream m_stream;
    Enumeration m_enum;

    public InputStreamInfo(String name, String handle, String mode, String eof_action)
    {
    	super(name, handle);
    	init(mode, eof_action);
    }

//    public InputStreamInfo(String name, String handle)
//    {
//    	super(name, handle);
//    	init();
//    }

//    public InputStreamInfo(String name)
//	{
//    	super(name);
//    	init();
//	}

    private void init(String mode, String eof_action)
    {
    	properties.setProperty("mode", "mode(" + mode + ")");
		properties.setProperty("input", "input");
		properties.setProperty("eof_action", "eof_action(" + eof_action + ")");
//		properties.setProperty("eof_action", "eof_action(eof_code)");
		properties.setProperty("reposition", "reposition(false)");
		properties.setProperty("end_of_stream", "end_of_stream(no)");
	}

//    private void init()
//    {
//    	properties.setProperty("mode", "mode(read)");
//		properties.setProperty("input", "input");
//		properties.setProperty("eof_action", "eof_action(reset)");
////		properties.setProperty("eof_action", "eof_action(eof_code)");
//		properties.setProperty("reposition", "reposition(false)");
//
//	}

    public int getLineNumber()
    {
    	if(m_stream == null)
    		return 0;

    	return m_stream.getLineNumber();
    }

    public int getPosition()
    {
    	if(m_stream == null)
    		return 0;

    	return m_stream.getRead();
    }

    public boolean isEOF() throws IOException
    {
    	if(m_stream == null)
    		return false;

    	m_stream.mark(1);

    	int i = m_stream.read();
    	if( i == -1 )
    		return true;

    	m_stream.pushback();

    	return false;
    }

}
