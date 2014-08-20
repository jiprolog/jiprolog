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

import java.util.*;
import java.io.*;
import java.net.*;

/**
 * <br>
 * Questa classe implementa un provider astratto di classi
 * @author       Ugo Chirico www.ugosweb.com
 */
public abstract class ClassProvider
{
    final private Hashtable m_cache;

    /**
    * Costruttore.
    */
    public ClassProvider()
    {
        m_cache = new Hashtable();
    }
    
    /**
    * Carica un file .class e lo restituisce come  array di byte.
    * @exception   ClassNotFoundException File .class non trovato
    * @exception   IOException Errore nella lettura del file.
    * @return   byte[]  array di byte contenente il file.
    */
    public byte[] loadClass(final String nameClass) throws ClassNotFoundException, IOException
    {
        if(m_cache.containsKey(nameClass))
            return (byte[])m_cache.get(nameClass);
        
        byte[] classBuffer = readClass(nameClass);
        
        m_cache.put(nameClass, classBuffer);
        
        return classBuffer;
    }
    
    /**
    * Carica una risorsa e lo restituisce come  array di byte.
    * @exception   IOException Errore nella lettura del file.
    * @return   byte[]  array di byte contenente la risorsa.
    */
    public byte[] loadResource(final String name) throws IOException
    {
        if(m_cache.containsKey(name))
            return (byte[])m_cache.get(name);
        
        byte[] classBuffer = readResource(name);
        
        m_cache.put(name, classBuffer);
        
        return classBuffer;
    }
    
    abstract byte[] readClass(final String nameClass)
        throws ClassNotFoundException, IOException;
    
    abstract byte[] readResource(final String nameResource)
        throws IOException;
    
        
     /**
     * Svuota la cache.
     */
    protected void finalize()
    {
        m_cache.clear();
        try
        {
            super.finalize();
        }
        catch (Throwable t) {}
    }
}
