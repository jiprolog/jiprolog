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

//import com.ugos.debug.*;
import java.io.*;
import java.util.*;

/**
 * <br>
 * Questa classe implementa una particolare estensione al classloader
 * @author       Ugo Chirico www.ugosweb.com
 */
public class ClassLoaderEx extends ClassLoader
{
    final private ClassProvider m_provider;
    
      /**
      * Costruttore.<br>
      *
      * @param  provider instanza di un ClassProvider che fornisce le classi.
      */
    public ClassLoaderEx(final ClassProvider provider)
    {
        m_provider = provider;
    }

    /**
    * Carica una classe.<br>
    * Il metodo viene invocato direttamente dal metodo pubblico loadClass(String name) in
    * java.lang.ClassLoader. <br>
    * @param     nameClass    nome della classe.
    * @param     resolve true o false se la classe va risolta o meno. In genere una classe e da
    *                        risolvere se contiene riferimeni ad altre classi.<br>
    *                        Il loader imposta correttamente ed automaticamente questo parametro ad ogni chiamata.<br>
    *
    * @exception ClassNotFoundException  classe non trovata.
    * @exception ClassFormatError    errore nel convertire la classe da array a  Class.
    * @return Class - la classe richiesta.
    */
    protected Class loadClass(String nameClass, boolean resolve)
        throws ClassNotFoundException, ClassFormatError
    {
        //Debug.traceln(nameClass, 1);
        
        byte[] classBytes = null;
        Class classClass = null;
        
        try
        {
        //Debug cerca la classe se già caricata
            classClass = findLoadedClassEx(nameClass);
        }
        catch(ClassNotFoundException cnfe)
        {
            try
            {
                // Carica la classe dal provider
                //Debug.traceln("loadClassFromProvider", 1);
                classBytes = loadClassFromProvider(nameClass);
                
                // Definisce il Package (solo JDK1.2 or later)
                try
                {
                    int dot = nameClass.lastIndexOf('.');
                    String pkgName = (dot < 0) ? null : nameClass.replace('/', '.').substring(0, dot);
                    if (pkgName != null && getPackage(pkgName) == null)
                    {
                        //Package p = this.definePackage(pkgName, null, null, null, null, null, null, null);
                        definePackage(pkgName, null, null, null, null, null, null, null);
                    }//end if there is a Package but it has not yet been defined
                }
                catch(Throwable th)
                {
                    // do nothing why maybe version prior to 1.2
                }

                //converte l'array di bytes in classe ed inserice la classe nella cache.
                classClass = defineClass(nameClass, classBytes, 0, classBytes.length);
                if (classClass == null)
                    throw new ClassFormatError(nameClass);
            }
            catch(IOException ex)
            {
                //ex.printStackTrace();
                throw new ClassNotFoundException(nameClass);
            }
//            catch(ClassNotFoundException ex)
//            {
//                ex.printStackTrace();
//                classClass = loadClassFromParent(nameClass);
//            }
       }
        
        
//        //System.out.println("Class richiesta: " + nameClass);
//        //Cerca la classe nella cache delle classi locali
//        classClass = findLoadedClass(nameClass);
//        if (classClass == null)
//        {
//            //System.out.println("Cerco la classe nel File System ... ");
//            try
//            {
//                classClass = findSystemClass(nameClass);
//            }
//            catch(ClassNotFoundException cnfe)
//            {
//                try
//                {
//                    classBytes = loadClassFromProvider(nameClass);
//                    //converte l'array di bytes in classe ed inserice la classe nella cache.
//                    classClass = defineClass(nameClass, classBytes, 0, classBytes.length);
//                    if (classClass == null)
//                        throw new ClassFormatError(nameClass);
//                }
//                catch(IOException ex)
//                {
//                    ex.printStackTrace();
//                    throw new ClassNotFoundException(nameClass);
//                }
//            }
//        }
            
        //se non e' risolta, risolve la classe
        if (resolve)
            resolveClass(classClass);
                
        //System.out.println("Class found:" + classClass);
        return classClass;
    }
      
    /**
    * Carica una classe dal provider e la restuituisce come array di byte.
    * @param     nameClass    nome della classe.
    * @exception ClassNotFoundException   Classe non trovata.
    * @exception IOException          see java.io.IOException.
    * @return        byte[] Array di byte che rappresenta la classe.
    */
    protected final byte[] loadClassFromProvider(final String nameClass)
            throws ClassNotFoundException, IOException
    {
        return m_provider.loadClass(nameClass);
    }
    
    /**
    * Cerca una risorsa dal provider e restuituisce il corrispondente inputstream.
    * @param     name    nome della risorsa.
    * @return    InputStream della risorsa.
    */
    public InputStream getResourceAsStream(String name)
    {
        InputStream ins = null;
        //System.out.println("Resource richiesta: " + name);
        
        try
        {
            byte[] buffer = m_provider.loadResource(name);
            if(buffer != null)
                ins = new ByteArrayInputStream(buffer);
        }
        catch(IOException ex)
        {
            
        }

        return ins;
    }

    private Class findLoadedClassEx(String name) throws ClassNotFoundException
    {
        Class classClass = null;
        
        //Debug.traceln("findCurrentClass", 1);
        // cerca la classe con il classloader corrente
        classClass = findCurrentClass(name);
        if (classClass == null)
        {
            //Debug.traceln("findLoadedClass", 1);
            //Cerca la classe nella cache delle classi locali
            classClass = findLoadedClass(name);
            if (classClass == null)
            {
                //System.out.println("Cerco la classe nel File System ... ");
                //Cerca la classe nel system class loader
                //Debug.traceln("findSystemClass", 1);
                classClass = findSystemClass(name);
            }
        }
        
        return classClass;
    }

    private Class findCurrentClass(String name)
    {
        Class classClass = null;
        ClassLoader clLoader = getClass().getClassLoader();
        
        //Debug.traceln("ClassLoader: " + clLoader, 1);
        if(clLoader != null)
        {
            try
            {
                classClass = clLoader.loadClass(name);
            }
            catch(ClassNotFoundException ex)
            {
                // Do nothing
            }
        }
        
        return classClass;

    }
//    public Class findLoadedClassEx(String name) throws ClassNotFoundException
//    {
//        Class classClass = null;
//
//        //Cerca la classe nella cache delle classi locali
//        classClass = findLoadedClass(name);
//
//        if (classClass == null)
//        {
//            //System.out.println("Cerco la classe nel File System ... ");
//            //Cerca la classe nel system class loader
//            try
//            {
//                classClass = findSystemClass(name);
//            }
//            catch(ClassNotFoundException cnfe)
//            {
//                //Cerca la classe nel class loader di this
//                ClassLoader clLoader = getClass().getClassLoader();
//              if(clLoader != null)
//              {
//                  classClass = clLoader.findLoadedClass(name);
//                  if(classClass == null)
//                      throw new ClassNotFoundException(name);
//              }
//                else
//                    throw new ClassNotFoundException(name);
//            }
//        }
//
//        return classClass;
//    }
}
