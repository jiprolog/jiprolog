package com.ugos.jiprolog.extensions.io;

import java.io.IOException;
import java.util.Enumeration;

import com.ugos.io.PushbackLineNumberInputStream;
import com.ugos.util.StringBuilderEx;

public class InputStreamInfo extends StreamInfo
{
    PushbackLineNumberInputStream m_stream;
    Enumeration m_enum;

    private static final StringBuilderEx sbMODE = new StringBuilderEx("mode(");

	private static int refCounter = 1;

    public InputStreamInfo(String name, int handle, String mode, String eof_action)
    {
    	super(name, handle != 0 ? handle : refCounter % MAX_VALUE);
    	refCounter+=2;
    	init(mode, eof_action);
    }

    private void init(String mode, String eof_action)
    {
    	properties.setProperty("mode", String.format("mode(%s)", mode));
		properties.setProperty("input", "input");
		properties.setProperty("eof_action", String.format("eof_action(%s)", eof_action));
//		properties.setProperty("eof_action", "eof_action(eof_code)");
		properties.setProperty("reposition", "reposition(false)");
		properties.setProperty("end_of_stream", "end_of_stream(not)");
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

    public int getColumn()
    {
    	if(m_stream == null)
    		return 0;

    	return m_stream.getColNumber();
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

    	int i = m_stream.read();
    	if( i == -1 )
    		return true;

    	m_stream.unread((char)i);

    	return false;
    }

}
