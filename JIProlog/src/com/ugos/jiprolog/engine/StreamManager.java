/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico
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

package com.ugos.jiprolog.engine;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
//import com.ugos.debug.*;

public class StreamManager
{
	private static StreamManager defaultStreamManager;
	private static StreamManager streamManager;

	static
	{
		defaultStreamManager = new StreamManager();
	}

	protected StreamManager()
	{

	}

	public static StreamManager getDefaultStreamManager()
	{
		return defaultStreamManager;
	}

	public static StreamManager getStreamManager()
	{
		if(streamManager == null)
		{
			return defaultStreamManager;
		}

		return streamManager;
	}

	public static void setStreamManager(StreamManager streamManager)
	{
		StreamManager.streamManager = streamManager;
	}

    public InputStream getInputStream(String strFilePath, String strBasePath, String[] strFileName, String[] strCurDir) throws IOException
    {
        InputStream ins = null;

        try
        {
            ins = getInputStream(strFilePath, strFileName, strCurDir);
        }
        catch(IOException ex)
        {
//        	ex.printStackTrace();

            // prova con il basepath
            if(strBasePath.toUpperCase().startsWith("JAR://"))
                ins = getInputStream(strBasePath + "#" + strFilePath, strFileName, strCurDir);
            else if(strBasePath.toUpperCase().startsWith("HTTP://") || strBasePath.toUpperCase().startsWith("HTTPS://"))
                ins = getInputStream(strBasePath + "/" + strFilePath, strFileName, strCurDir);
            else if(strBasePath.toUpperCase().startsWith("INTERNAL://"))
                ins = getInputStream(strBasePath + "/" + strFilePath, strFileName, strCurDir);
            else
                ins = getInputStream(strBasePath + File.separator + strFilePath, strFileName, strCurDir);
        }

        return ins;
    }

    public OutputStream getOutputStream(String strFilePath, String strBasePath, boolean bAppend, String[] strFileName, String[] strCurDir) throws IOException
    {
        OutputStream outs = null;

        try
        {
            outs = getOutputStream(strFilePath, bAppend, strFileName, strCurDir);
        }
        catch(IOException ex)
        {
            // prova con il basepath
            if(strBasePath.toUpperCase().startsWith("JAR://"))
                outs = getOutputStream(strBasePath + "#" + strFilePath, bAppend, strFileName, strCurDir);
            else if(strBasePath.toUpperCase().startsWith("HTTP://"))
                outs = getOutputStream(strBasePath + "/" + strFilePath, bAppend, strFileName, strCurDir);
            else
                outs = getOutputStream(strBasePath + File.separator + strFilePath, bAppend, strFileName, strCurDir);
        }

//        try
//        {
//
//            //if(strFilePath.startsWith(File.separator))
//            if(new File(strFilePath).isAbsolute())
//                outs = getOutputStream(strFilePath, bAppend, strFileName, strCurDir);
//            // prova con basepath
//            else if(strBasePath.toUpperCase().startsWith("JAR://"))
//                outs = getOutputStream(strBasePath + "#" + strFilePath, bAppend, strFileName, strCurDir);
//            else
//                outs = getOutputStream(strBasePath + File.separator + strFilePath, bAppend, strFileName, strCurDir);
//        }
//        catch(Throwable ex)
//        {
//            //prova con path assoluto
//            outs = getOutputStream(strFilePath, bAppend, strFileName, strCurDir);
//        }

        return outs;
    }

    //curDir contiene la dir corrente con separatore finale
    // strFileName contiene il nome del file
    //public static InputStream getInputStream(String strFilePath, String strBasePath, String[] strFileName, String[] strCurDir) throws IOException
    private InputStream getInputStream(String strPath, String[] strFileName, String[] strCurDir) throws IOException, SecurityException
    {
        // elimina eventuali apici
        if(strPath.charAt(0) == 39 || strPath.charAt(0) == 34)
        {
            strPath = strPath.substring(1, strPath.length() - 1);
        }

        // controlla il protocollo jar, http, file o nulla
        if(strPath.toUpperCase().startsWith("HTTP://") || strPath.toUpperCase().startsWith("HTTPS://"))
        {
    //      Debug.traceln("url:" + strPath, 1);
            // prova con url
            URL url = new URL(strPath);
            strFileName[0] = strPath.substring(7);
            strCurDir[0] = "http://" + url.getHost() + File.separatorChar;
            return url.openStream();
        }
        else if(strPath.toUpperCase().startsWith("FILE:/"))
        {
            // prova con url
        	URI uri;
        	URL url;
			try {
				uri = new URI(strPath);
				url = uri.toURL();
			} catch (URISyntaxException e) {
				url = new URL(strPath);
			}

			// sostituisce il separatorChar per uniformare windows a unix
            strPath = strPath.substring(6).replace('\\', File.separatorChar);
            strPath = strPath.replace('/', File.separatorChar);
            strFileName[0] = strPath;//.substring(6);
            int nSepPos = strPath.lastIndexOf(File.separatorChar);
            if(nSepPos < 0)
            {
                //strPath = strPath.replace('\\', '/');
                throw new FileNotFoundException(strPath);
            }
            strCurDir[0] = "FILE:/" + strPath.substring(0, nSepPos) + File.separatorChar;
            //            Debug.traceln("fileName:" + strName, 1);
            //            Debug.traceln("curdir:" + strCuPath, 1);
            return url.openStream();
        }
        else if(strPath.toUpperCase().startsWith("JAR://"))
        {
            // prova con zip
            int nSepPos = strPath.indexOf('#');
            if(nSepPos < 0)
            {
                //strPath = strPath.replace('\\', '/');
                throw new FileNotFoundException(strPath);
            }
            String strFile = strPath.substring(nSepPos + 1);
            //Debug.traceln("file:" + strFile, 1);
            strPath = strPath.substring(6, nSepPos);
            //Debug.traceln("jar:" + strPath, 1);
            strCurDir[0] = strFile + "#";
            strFileName[0] = strPath;
            ZipFile zipFile = new ZipFile(strPath);
            ByteArrayOutputStream outs;
            try
            {
                // try unix format
                strFile = strFile.replace((char)92, '/');
                ZipEntry entry = zipFile.getEntry(strFile);
                if(entry == null)
                {
                    // try win format
                    strFile = strFile.replace('/', (char)92);
                    entry = zipFile.getEntry(strFile);
                }

                if(entry == null)
                {
                    strPath = strPath.replace('\\', '/');
                    throw new FileNotFoundException(strPath);
                }

                Reader ins = new InputStreamReader(zipFile.getInputStream(entry));
                // legge il file
                outs = new ByteArrayOutputStream();
                int c;
                while((c = ins.read()) != -1)
                {
                    outs.write(c);
                }
                // chiude il file zip
                zipFile.close();
            }
            catch(IOException ex)
            {
                zipFile.close();
                throw ex;
            }

    //              Debug.traceln(new String(outs.toByteArray()), 1);
            // crea un nuovo input stream
            ByteArrayInputStream bins = new ByteArrayInputStream(outs.toByteArray());

            return bins;
        }
        else if(strPath.toUpperCase().startsWith("INTERNAL://"))
        {
        	strCurDir[0] = strPath.substring(0, strPath.lastIndexOf('/'));

            // sostituisce il separatorChar per uniformare windows a unix
            strPath = strPath.substring(11);
            strFileName[0] = strPath;//.substring(6);

            InputStream ins = StreamManager.class.getResourceAsStream("/" + strPath);
            return ins;
        }
	    else
	    {
	        /////////////////
	        // E' un file
	        /////////////////

	        File file = new File(new File(strPath).getCanonicalPath());

	        strFileName[0] = file.getCanonicalPath();//strPath;
	        strCurDir[0]   = file.getParent() + File.separatorChar;
	        return new FileInputStream(file);
	    }
    }

    //curDir contiene la dir corrente con separatore finale
    private OutputStream getOutputStream(String strPath, boolean bAppend, String[] strFileName, String[] strCurDir) throws IOException
    {
        // elimina eventuali apici
        if(strPath.charAt(0) == 39 || strPath.charAt(0) == 34)
        {
            strPath = strPath.substring(1, strPath.length() - 1);
        }

        // controlla il protocollo jar, http, file o nulla
        if(strPath.toUpperCase().startsWith("HTTP://") || strPath.toUpperCase().startsWith("HTTPS://"))
        {
            //strPath = strPath.replace('\\', '/');
        	throw new JIPTypeException(JIPTypeException.URL, strPath);
//            throw JIPRuntimeException.create(20, strPath);
        }
        else if(strPath.toUpperCase().startsWith("FILE:/"))
        {
            // prova con url
            URL url = new URL(strPath);
            strFileName[0] = strPath.substring(8);
            //strPath = strPath.substring(7);
            int nSepPos = strPath.lastIndexOf(File.separatorChar);
            if(nSepPos < 0)
            {
                //strPath = strPath.replace('\\', '/');
                throw new FileNotFoundException(strPath);
            }
            strCurDir[0] = strPath.substring(0, nSepPos) + File.separatorChar;
//              Debug.traceln("fileName:" + strName, 1);
//              Debug.traceln("curdir:" + strCuPath, 1);
            return url.openConnection().getOutputStream();
        }
        else if(strPath.toUpperCase().startsWith("JAR://"))
        {
            //strPath = strPath.replace('\\', '/');
        	throw new JIPTypeException(JIPTypeException.FILE, strPath);
//            throw JIPRuntimeException.create(20, strPath);
        }
        else
        {
            /////////////////
            // E' un file
            /////////////////
            File file = new File(new File(strPath).getCanonicalPath());
            strFileName[0] = file.getCanonicalPath();//strPath;
            strCurDir[0]   = file.getParent() + File.separatorChar;
            return new FileOutputStream(file.getAbsolutePath(), bAppend);
        }
    }
}

