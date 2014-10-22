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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.zip.ZipFile;
import java.util.*;

final class SearchPath1 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        PrologObject param = getParam(1);
        if (param instanceof Variable)
        {
            if (!((Variable)param).isBounded())
            {
                if(getJIPEngine().getSearchPath() == null)
                    throw JIPRuntimeException.create(26, null);
                
                return param.unify(Atom.createAtom(getJIPEngine().getSearchPath()), varsTbl);
            }
            else
            {
                param = ((Variable)param).getObject();
            }
        }
        
        String strSearchDir;
        if (param instanceof Atom)
        {
            strSearchDir = ((Atom)param).getName();
        }
        else if (param instanceof PString)
        {
            strSearchDir = ((PString)param).getString();
        }
        else
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM_OR_STRING);
        }
        
        try
        {
            // prova con basepath
            String strBasePath = getJIPEngine().getSearchPath();
            
            if(new File(strSearchDir).isAbsolute())
                strSearchDir = getValidSearchPath(strSearchDir);
            else if(strBasePath.toUpperCase().startsWith("JAR://"))
                strSearchDir = getValidSearchPath(strBasePath + "#" + strSearchDir);
            else
                strSearchDir = getValidSearchPath(strBasePath + File.separator + strSearchDir);
        }
        catch(Throwable ex)
        {
            try
            {
                //prova con path assoluto
                strSearchDir = getValidSearchPath(strSearchDir);
            }
            catch (FileNotFoundException ex1)
            {
                throw JIPRuntimeException.create(6, strSearchDir);
            }
        }
        
        try
        {
            getJIPEngine().setSearchPath(strSearchDir);
        }
        catch(IOException ex)
        {
            throw new JIPJVMException(ex);
        }

        return true;
    }

    static final String getValidSearchPath(String strSearchDir) throws FileNotFoundException
    {
        if(strSearchDir == null)
            return null;
        
        //System.out.println("getValidSearchPath " + strSearchDir);
        
        // elimina apici
        if(strSearchDir.charAt(0) == 39 || strSearchDir.charAt(0) == 34)
        {
            strSearchDir = strSearchDir.substring(1, strSearchDir.length() - 1);
        }
    
        // controlla il protocollo jar, http, file o nulla
        if(strSearchDir.toUpperCase().startsWith("HTTP://"))
        {
            try
            {
                URL url = new URL(strSearchDir);
            }
            catch(MalformedURLException ex)
            {
                throw new JIPJVMException(ex);
            }
            catch(SecurityException ex)
            {
                throw JIPRuntimeException.create(9, null);
            }
                            
            return strSearchDir;
        }
        else if(strSearchDir.toUpperCase().startsWith("JAR://"))
        {
            // prova con zip
            String strPath = strSearchDir.substring(6);
            //Debug.traceln("jar:" + strPath, 1);
            try
            {
                ZipFile zipFile = new ZipFile(strPath);
                zipFile.close();
                return strSearchDir;
            }
            catch(SecurityException ex)
            {
                throw JIPRuntimeException.create(9, null);
            }
            catch(FileNotFoundException ex)
            {
                throw JIPRuntimeException.create(18, null);
            }
            catch(IOException ex)
            {
                throw new JIPJVMException(ex );
            }
        }
//        else if(strSearchDir.toUpperCase().startsWith("FILE:///ANDROID_ASSET/"))
//        {
//        	return strSearchDir;
//        }
        else if(strSearchDir.toUpperCase().startsWith("FILE:/"))
        {
            // prova con url
            //strSearchDir = strSearchDir.substring(6);
        
            // prova con url
//            try
//            {
//                URL url = new URL(strSearchDir);
//                InputStream ins = url.openStream();
//                ins.close();
//            }
//            catch(MalformedURLException ex)
//            {
//                throw new JIPJVMException(ex);
//            }
//            catch(SecurityException ex)
//            {
//                throw JIPRuntimeException.create(9, "Cannot access the directory " + strSearchDir);
//            }
//            catch(FileNotFoundException ex)
//            {
//                throw ex;
//            }
//            catch(IOException ex)
//            {
//                throw new JIPJVMException(ex);
//            }

            // sostituisce il separatorChar per uniformare windows a unix
                        
            return strSearchDir;
        }
        else if(strSearchDir.toUpperCase().startsWith("INTERNAL://"))
        {
            return strSearchDir;
        }
        
//        System.out.println(strSearchDir);  // DBG
        
//      System.out.println(strSearchDir);
        strSearchDir = strSearchDir.replace('\\', File.separatorChar);
        strSearchDir = strSearchDir.replace('/', File.separatorChar);
        
//        System.out.println(strSearchDir);  // DBG
        
//      System.out.println(strSearchDir.charAt(strSearchDir.length() - 1));
        // controlla se l'ultimo carattere è separator
//        if(strSearchDir.charAt(strSearchDir.length() - 1) == File.separatorChar)
//            strSearchDir.substring(0, strSearchDir.length() - 1);
                        
//    System.out.println(strSearchDir);  // DBG
        
//        // patch per Pocket PC
//        if(strSearchDir.equals("\\\\"))
//            strSearchDir = "\\";
        
//    System.out.println(strSearchDir);  // DBG
        
        
        return strSearchDir;
//        try
//        {
//            File file = new File(new File(strSearchDir).getCanonicalPath());
//            String strAbsPath = file.getCanonicalPath();
//            if(file.exists() && file.isDirectory())
//            {
////                if(strAbsPath.charAt(strAbsPath.length() - 1) == File.separatorChar)
//                    strSearchDir = strAbsPath;
////                else
////                    strSearchDir = strAbsPath + File.separator;
//                                
//                return strSearchDir;
//            }
//            else
//            {
//                throw new FileNotFoundException(strSearchDir);
//            }
//        }
//        catch(SecurityException ex)
//        {
//            throw JIPRuntimeException.create(9, "Cannot access the directory " + strSearchDir);
//        }
//        catch(FileNotFoundException ex)
//        {
//            throw ex;
//        }
//        catch(IOException ex)
//        {
//            throw new JIPJVMException(ex);
//        }
    }
}


