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

package com.ugos.jiprolog;

import java.awt.Frame;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Properties;

import com.ugos.jiprolog.engine.JIPDebugger;
import com.ugos.jiprolog.engine.JIPEngine;
import com.ugos.jiprolog.engine.JIPQuery;
import com.ugos.jiprolog.engine.JIPRuntimeException;
import com.ugos.jiprolog.engine.JIPSyntaxErrorException;
import com.ugos.jiprolog.engine.JIPTerm;


/**
 * JIProlog implements the stand alone prolog interpreter.<br>
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 */
public class JIProlog
{
    private static final String VERSION = "3.2";

    private static JIPEngine jip;

    /** Entry point of the stand alone prolog interpreter
     * @param args the first element of the array must be the file to consult.<br>
     * Note that such a file must define main/0 predicate.
     */
    public static void main(String args[])
    {
        System.out.println("*************************************************");
        System.out.println("** JIProlog Standalone Interpreter v" + VERSION);
        System.out.println("** Based on JIProlog v" + JIPEngine.getVersion());
        System.out.println("** " + JIPEngine.getCopyrightInfo());
        System.out.println("** http://www.jiprolog.com");
        System.out.println("*************************************************\n\n");


        try
        {
        	processArgs(args);

            System.out.println("\n\n***************************************************");
            System.out.println("** Thanks for using JIProlog                     **");
            System.out.println("***************************************************");
        }
        catch(IOException ex)
        {
            showMessage(ex.getMessage());
        }
        catch(JIPSyntaxErrorException ex)
        {
            showMessage(ex.getMessage());
        }
        catch(JIPRuntimeException ex)
        {
            showMessage(ex.getMessage());
        }
    }

    private static void showMessage(String strMsg)
    {
    	System.out.println(strMsg);
    }

    public static void printHelp()
    {
    	System.out.println("*************************************************");
        System.out.println("** JIProlog Standalone interpreter v" + VERSION);
        System.out.println("** Based on JIProlog v" + JIPEngine.getVersion());
        System.out.println("** " + JIPEngine.getCopyrightInfo());
        System.out.println("*************************************************");
        System.out.println("\nOptions:");
        System.out.println("\n -debug to run JIProlog in debug mode");
        System.out.println("\n -c <file> to compile a prolog file");
        System.out.println("\n -g <goal> initialization goal");
        System.out.println("\n -d <path> working directory");
    }

    private static void processArgs(String args[]) throws JIPSyntaxErrorException, IOException
    {
    	String initializationGoal = null;

    	if(args.length == 1)
    	{
    		if(args[0].startsWith("-h"))
    		{
    			printHelp();
    			return;
    		}
    		else if(args[0].startsWith("-version"))
        	{
    	        System.out.println("JIProlog Standalone Interpreter v" + VERSION);
    	        System.out.println("Based on JIProlog v" + JIPEngine.getVersion());
        	}
    		else if(args[0].startsWith("-debug"))
        	{
        		JIPDebugger.debug = true;
        	}
    	}
    	else
    	{
	        for(int i = 0; i < args.length; i++)
	        {
	        	if(args[i].startsWith("-debug"))
	        	{
	        		JIPDebugger.debug = true;
	        	}
	        	else if(args[i].startsWith("-c"))
	        	{
	        		if(i + 1 < args.length)
	        		{
	        			if(jip == null)
	        				jip = new JIPEngine();

	        			System.out.println("consulting file " + args[i+1]);
	        			jip.consultFile(args[i + 1]);
	        			i++;
	        		}
	        	}
	        	else if(args[i].startsWith("-g"))
	        	{
	        		if(i + 1 < args.length)
	        		{
	        			initializationGoal = args[i + 1];
	        			i++;
	        		}
	        	}
	        	else if(args[i].startsWith("-d"))
	        	{
					try
	        		{
		        		if(i + 1 < args.length)
		        		{
		        			if(jip == null)
		        				jip = new JIPEngine();

		        			jip.setSearchPath(args[i + 1]);
		        			i++;
		        		}
					}
	        		catch (IOException e)
	        		{
						e.printStackTrace();
					}
	        	}
	        }
    	}

		if(jip == null)
			jip = new JIPEngine();

    	if(initializationGoal != null)
    	{
    		System.out.println("running goal " + initializationGoal);

    		JIPQuery query = jip.openSynchronousQuery(initializationGoal);
    		JIPTerm term = query.nextSolution();
    	}
    }
}

