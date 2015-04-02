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
import java.nio.file.FileAlreadyExistsException;
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
                	return param.unify(List.NIL, varsTbl);

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
            throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, param);
        }

        try
        {
            String strBasePath = getJIPEngine().getSearchPath();

			strSearchDir = getValidSearchPath(strSearchDir, strBasePath);
		}
        catch (FileNotFoundException e)
        {
        	throw JIPExistenceException.createSourceSynkException(Atom.createAtom(strSearchDir));
//        	 throw JIPRuntimeException.create(18, strSearchDir);
	    }
	    catch(MalformedURLException ex)
	    {
	        throw new JIPJVMException(ex);
	    }
	    catch(SecurityException ex)
	    {
	    	throw new JIPPermissionException("access", "source_sink", Atom.createAtom(strSearchDir));
//	        throw JIPRuntimeException.create(9, ex.getMessage());
	    }
        catch (IOException ex)
        {
        	throw new JIPJVMException(ex);
		}


//        try
//        {
//            // prova con basepath
//            String strBasePath = getJIPEngine().getSearchPath();
//
//            if(new File(strSearchDir).isAbsolute())
//                strSearchDir = getValidSearchPath(strSearchDir);
//            else if(strBasePath.toUpperCase().startsWith("JAR://"))
//                strSearchDir = getValidSearchPath(strBasePath + "#" + strSearchDir);
//            else
//                strSearchDir = getValidSearchPath(strBasePath + File.separator + strSearchDir);
//        }
//        catch(Throwable ex)
//        {
//            try
//            {
//                //prova con path assoluto
//                strSearchDir = getValidSearchPath(strSearchDir);
//            }
//            catch (FileNotFoundException ex1)
//            {
//                throw JIPRuntimeException.create(6, strSearchDir);
//            }
//        }

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

    static final String getValidSearchPath(String strSearchDir, String basePath) throws IOException
    {
        if(strSearchDir == null)
            return null;

        // elimina apici
        if(strSearchDir.charAt(0) == 39 || strSearchDir.charAt(0) == 34)
        {
            strSearchDir = strSearchDir.substring(1, strSearchDir.length() - 1);
        }

        // controlla il protocollo jar, http, file o nulla
        if(strSearchDir.toUpperCase().startsWith("HTTP://") || strSearchDir.toUpperCase().startsWith("HTTPS://"))
        {
            URL url = new URL(strSearchDir);

            return strSearchDir;
        }
        else if(strSearchDir.toUpperCase().startsWith("JAR://"))
        {
            // prova con zip
            String strPath = strSearchDir.substring(6);
            ZipFile zipFile = new ZipFile(strPath);
            zipFile.close();
            return strSearchDir;
        }
        else if(strSearchDir.toUpperCase().startsWith("INTERNAL://"))
        {
            return strSearchDir;
        }
//      else if(strSearchDir.toUpperCase().startsWith("FILE:///ANDROID_ASSET/"))
//      {
//      	return strSearchDir;
//      }
        else if(strSearchDir.toUpperCase().startsWith("FILE:/"))
        {
        	strSearchDir = strSearchDir.replace('\\', File.separatorChar);
	        strSearchDir = strSearchDir.replace('/', File.separatorChar);

	        String path = strSearchDir.substring(6);

	        File f = new File(path);

			if(!f.exists())
			{
				throw new FileNotFoundException(path);
			}

			return f.getAbsolutePath();
        }
        else
        {
        	strSearchDir = strSearchDir.replace('\\', File.separatorChar);
	        strSearchDir = strSearchDir.replace('/', File.separatorChar);

	        File f = new File(strSearchDir);
	        if(f.isAbsolute())
	        {
	        	if(!f.exists())
	        		throw new FileNotFoundException(strSearchDir);
	        	else
	        		return f.getAbsolutePath();
	        }
	        else if(strSearchDir.equals(".."))
	        {
	        	return new File(basePath).getParentFile().getAbsolutePath();
	        }
	        else
	        {
	        	f = new File(basePath, strSearchDir);
	        	if(!f.exists())
				{
	            	String runtime = System.getProperty("java.runtime.name").toLowerCase();

//	    			System.out.println ("runtime=" + runtime);

	    			if(runtime.contains("android"))
	    			{
	    		        return strSearchDir;
	    			}
	    			else
	    			{
	    				throw new FileNotFoundException(strSearchDir);
	    			}
				}

	        	return f.getAbsolutePath();
	        }
        }
    }
}


