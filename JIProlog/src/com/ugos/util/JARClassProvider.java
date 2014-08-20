/*************************
 *
 * 09/19/2002
 *
 * Copyright (C) 2002 Ugo Chirico
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
 **************************/

package com.ugos.util;

import java.io.*;
import java.util.*;
import java.util.zip.*;

/**
 * Questa classe implementa un ClassProvider per file .jar
 * @author       Ugo Chirico www.ugosweb.com
 */
public class JARClassProvider extends ClassProvider
{
    // vettore dei jar riconosciuti
    final private Vector m_jarVect;

    /**
    * Costruttore.
    */
    public JARClassProvider()
    {
        super();
        m_jarVect = new Vector();
    }
    
    /**
    * Aggiunge un path al provider.
    */
    public void addJarPath(final String strJarPath)
    {
        synchronized(m_jarVect)
        {
            m_jarVect.addElement(strJarPath);
        }
    }
    
    /**
    * Rimuove un path al provider.
    */
    public void removeJarPath(final String strJarPath)
    {
        synchronized(m_jarVect)
        {
            m_jarVect.removeElement(strJarPath);
        }
    }
    
    /**
    * Legge una classe da uno dei jar in lista.
    */
    protected byte[] readClass(final String nameClass) throws ClassNotFoundException, IOException
    {
        // cerca la classe in tutti i JARs in m_jarVect
        byte[] classBuffer = null;
        int i = 0;
                
        synchronized(m_jarVect)
        {
            // cerca l'entry in tutti i JARs in m_jarVect
            while(classBuffer == null && i < m_jarVect.size())
            {
                // try win format
                String nameClass1 = nameClass.replace('.', '\\') + ".class";
                classBuffer = readEntryFromJar((String)m_jarVect.elementAt(i), nameClass1);
                if(classBuffer == null)
                {
                    // try unix format
                    nameClass1 = nameClass.replace('.', '/') + ".class";
                    classBuffer = readEntryFromJar((String)m_jarVect.elementAt(i), nameClass1);
                }
                i++;
            }
        }
        
        if(classBuffer == null)
            throw new ClassNotFoundException(nameClass);
        
        return classBuffer;
    }
    
    /**
     * Legge una risorsa da uno dei jar in lista.
    */
    protected byte[] readResource(final String nameResource) throws IOException
    {
        byte[] classBuffer = null;
        int i = 0;

        synchronized(m_jarVect)
        {
            // cerca l'entry in tutti i JARs in m_jarVect
            while(classBuffer == null && i < m_jarVect.size())
            {
                classBuffer = readEntryFromJar((String)m_jarVect.elementAt(i), nameResource);
                i++;
            }
        }
        
        return classBuffer;
    }
             
    /**
     * Legge una singola entry dal jar specificato.
    */
    private static byte[] readEntryFromJar(String jar, String name) throws IOException
    {
        //System.out.println("Search for " + name);
    
        ZipFile zipFile = new ZipFile(jar);
        ZipEntry entry = zipFile.getEntry(name);
        if(entry == null)
        {
            entry = zipFile.getEntry(name);
            if(entry == null)
            {
                zipFile.close();
                return null;
            }
        }
    
        //System.out.println("Resource found");
    
        ByteArrayOutputStream outs = new ByteArrayOutputStream();
        BufferedInputStream ins = new BufferedInputStream(zipFile.getInputStream(entry));
        
        int c = ins.read();
        while(c != -1)
        {
            outs.write(c);
            c = ins.read();
        }
    
        ins.close();
        zipFile.close();
        return outs.toByteArray();
    }

}
