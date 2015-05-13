package com.ugos.jiprolog.extensions.io;

import java.io.OutputStream;

public class OutputStreamInfo extends StreamInfo
{
	private static int refCounter = 2;
	private static int MAX_VALUE = Integer.MAX_VALUE - 1;
	OutputStream m_stream;

    public OutputStreamInfo(String name, int handle, String mode)
	{
    	super(name, handle != 0 ? handle : refCounter % MAX_VALUE);
    	refCounter+=2;
    	init(mode);
	}

    private void init(String mode)
    {
    	properties.setProperty("mode", String.format("mode(%s)", mode));
		properties.setProperty("output", "output");
		properties.setProperty("reposition", "reposition(false)");
		properties.setProperty("eof_action", "eof_action(reset)");
	}
}
