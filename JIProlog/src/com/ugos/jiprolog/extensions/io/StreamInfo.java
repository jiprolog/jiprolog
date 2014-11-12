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
	}

	public String getName()
	{
		return name;
	}

	public String getHandle()
	{
		return name;
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


}

