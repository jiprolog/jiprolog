package com.ugos.jiprolog.extensions.io;

import java.io.OutputStream;

public class OutputStreamInfo extends StreamInfo
{
	private static int refCounter = 1;

	OutputStream m_stream;

    public OutputStreamInfo(String name, String handle, String mode)
	{
    	super(name, handle != null ? handle : "out."+ refCounter % Integer.MAX_VALUE);
    	refCounter++;
    	init(mode);
	}

    private void init(String mode)
    {
    	properties.setProperty("mode", "mode(" + mode + ")");
		properties.setProperty("output", "output");
		properties.setProperty("reposition", "reposition(false)");
		properties.setProperty("eof_action", "eof_action(reset)");
	}
}
