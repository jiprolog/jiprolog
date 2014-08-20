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

package com.ugos.JIProlog.extensions.system;

import com.ugos.JIProlog.engine.*;
import java.io.*;
import java.util.*;

public class Statistics0 extends JIPXCall
{
    public final boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        PrintStream outs = new PrintStream(getJIPEngine().getCurrentOutputStream());
                
        outs.println("\nStatistics:");

        outs.print("   Total Memory: ");
        outs.println(Long.toString(Runtime.getRuntime().totalMemory()));
        outs.print("   Free Memory:  ");
        outs.println(Long.toString(Runtime.getRuntime().freeMemory()));
        outs.print("   Current Output Stream:  ");
        outs.println(getJIPEngine().getCurrentOutputStreamName());
        outs.print("   Current Input Stream:   ");
        outs.println(getJIPEngine().getCurrentInputStreamName() + "\n");
        
        outs.flush();
        
        return true;
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

