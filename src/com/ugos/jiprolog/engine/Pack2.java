/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico - http://www.ugochirico.com
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.util.*;

import com.ugos.io.PushbackLineNumberInputStream;

//import com.ugos.util.MapperHolder;

final class Pack2 extends BuiltIn
{
    public final boolean unify(final Hashtable<Variable, Variable> varsTbl)
    {
        String strDestinationFile = null;

        final PrologObject fileList = getRealTerm(getParam(1));
        final PrologObject destinationFile = getRealTerm(getParam(2));


        if(!(fileList instanceof List))
            throw new JIPTypeException(JIPTypeException.LIST, fileList);


        if(destinationFile instanceof Atom)
        {
        	strDestinationFile = ((Atom)destinationFile).getName();
        }
        else if(destinationFile instanceof PString)
        {
        	strDestinationFile = ((PString)destinationFile).getString();
        }
        else if(destinationFile.unifiable(List.NIL))
        {
            throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, destinationFile);
        }

        try
        {
			pack((List)fileList, strDestinationFile, getJIPEngine());
		}
        catch (FileNotFoundException e)
        {
            throw JIPExistenceException.createSourceSynkException(destinationFile);
		}
        catch (IOException e)
        {
        	throw new JIPJVMException(e);
		}

        return true;
    }

    public static final void pack(List fileList, String destinationFile, final JIPEngine engine) throws FileNotFoundException, IOException
    {
        InputStream ins = null;

        File outf = new File(destinationFile);
        if(!outf.isAbsolute())
        	outf = new File(engine.getSearchPath(), destinationFile);

    	final ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(outf));

        String strPath = null;
        int len = fileList.getHeight();
        for(int i = 1; i <= len; i++)
        {
	        try
	        {
	        	PrologObject path = getRealTerm(fileList.getTerm(i));
	        	if(!(path instanceof Atom))
	                throw new JIPTypeException(JIPTypeException.ATOM_OR_STRING, path);

	        	strPath = ((Atom)path).getName();
	            String strFileName[] = new String[1];
	            String strCurDir[] = new String[1];
	            ins = StreamManager.getStreamManager().getInputStream(strPath, engine.getSearchPath(), strFileName, strCurDir);

	            String file = new File(strPath).getName();
	            final int nPos = file.lastIndexOf('.');
	            file = new StringBuilder(file.substring(0, nPos)).append(".jip").toString();
	            PrologParser parser = new PrologParser(new ParserReader(new PushbackLineNumberInputStream(ins)), engine.getOperatorManager(), engine, strPath);

	            try
	            {
	                PrologObject term;

	                while ((term = parser.parseNext()) != null)
	                {
	                    out.writeObject(term);
	                }

	                ins.close();
	                ins = null;
	            }
	            catch(IOException ex)
	            {
		            ins.close();
		            ins = null;
		            out.close();

	            	ex.printStackTrace();
	                throw new JIPJVMException(ex);
	            }
	        }
	        catch(FileNotFoundException ex)
	        {
	            try
	            {
	                if(ins != null)
	                    ins.close();

		            out.close();

	            }
	            catch(IOException ex1){};

	            throw JIPExistenceException.createSourceSynkException(Atom.createAtom(strPath));
	        }
	        catch(IOException ex)
	        {
	            try
	            {
	                if(ins != null)
	                    ins.close();
	            }
	            catch(IOException ex1){}

	            throw new JIPJVMException(ex);
	        }
	        catch(SecurityException ex)
	        {
	            try
	            {
	                if(ins != null)
	                    ins.close();
	            }
	            catch(IOException ex1){}

	            throw new JIPPermissionException("access", "source_sink", Atom.createAtom(strPath));
	        }
	        catch(JIPRuntimeException ex)
	        {
	            ex.m_strFileName = strPath;


	            throw ex;
	        }
	        finally
	        {
	            try
	            {
	                if(ins != null)
	                    ins.close();
	            }
	            catch(IOException ex1){}
	        }
        }

        out.close();
    }
}
