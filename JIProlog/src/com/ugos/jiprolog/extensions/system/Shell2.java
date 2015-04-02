/*****************************************
 * 27/03/2003
 *
 * Copyright (C) 1999-2003 Ugo Chirico
 * http://www.ugochirico.com
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
 *****************************************/

package com.ugos.jiprolog.extensions.system;

import com.ugos.jiprolog.engine.*;

import java.util.*;
import java.io.*;

public class Shell2 extends JIPXCall
{
    public final boolean unify(final JIPCons input, Hashtable varsTbl)
    {
        JIPTerm term = input.getNth(1);

        // check if input is a variable
        if (term instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)term).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                term = ((JIPVariable)term).getValue();
            }
        }
        if (!(term instanceof JIPAtom))
        {
            throw new JIPTypeException(JIPTypeException.LIST, term);
        }

        JIPAtom commands = (JIPAtom)term;

        Process proc;

        try
        {
        	String runtime = System.getProperty("java.runtime.name").toLowerCase();
        	String os = System.getProperty ("os.name").toLowerCase();
//			String arch = System.getProperty ("os.arch").toLowerCase();

			if(os.contains("win"))
			{
				String cmd = "cmd.exe /C " + commands.getName();

		        proc = Runtime.getRuntime().exec(cmd);

			}
			else if(runtime.contains("android"))
			{
				proc = Runtime.getRuntime().exec(commands.getName());
			}
			else // linux mac //if(os.contains("mac") || os.contains("darwin") || os.contains("linux))
			{
				String shell  = System.getenv("SHELL");
				System.out.println("SHELL ENV: " + shell);


				if(shell == null)
				{
					String[] cmd = { "/bin/sh", "-c", commands.getName() };
			        proc = Runtime.getRuntime().exec(cmd);
				}
				else
				{
					String[] cmd = { shell, "-c", commands.getName() };
			        proc = Runtime.getRuntime().exec(cmd);
				}
			}
//			else // linux
//			{
//				if(runtime.contains("android"))
//				{
//					// TODO
//				}
//				else
//				{
//					String shell  = System.getenv("SHELL");
//					System.out.println("SHELL ENV: " + shell);
//					args[0] = shell;
//					args[1] = "-c";
//				}
//			}


            int nExit = proc.waitFor();

            return input.getNth(2).unify(JIPNumber.create(nExit), varsTbl);
        }
        catch(Exception ex)
        {
        	ex.printStackTrace();
            throw new JIPJVMException(ex);
        }
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

