package com.ugos.jiprolog.extensions.io;

import java.util.Properties;


public class StreamInfo
{
    private String name;
    private String handle;
	protected Properties properties = new Properties();

	public StreamInfo(String name)
	{
		this(name, "#" + name.hashCode());
	}

	public StreamInfo(String name, String handle)
	{
		this.name = name;
		this.handle = handle;

		properties.setProperty("file_name", "file_name('" + name.replace("\\", "/") + "')");
		properties.setProperty("alias", "alias('" + handle + "')");
		properties.setProperty("type", "type(text)");
		properties.setProperty("end_of_stream", "end_of_stream(no)");
	}

	public String getName()
	{
		return name;
	}

	public String getHandle()
	{
		return handle;
	}

	public void setHandle(String handle)
	{
		this.handle = handle;
	}

	public Properties getProperties()
	{
		return properties;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public void setEndOfStream(String pos)
	{
		properties.setProperty("end_of_stream", "end_of_stream(" + pos + ")");
	}

	public String getEndOfStream()
	{
		return properties.getProperty("end_of_stream", "no");
	}

}

