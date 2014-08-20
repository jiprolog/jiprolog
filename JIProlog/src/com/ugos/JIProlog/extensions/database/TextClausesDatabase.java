/*
 * 09/19/2002
 *
 * Copyright (C) 1999-2003 Ugo Chirico
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

package com.ugos.JIProlog.extensions.database;

import com.ugos.JIProlog.engine.*;
import java.util.Enumeration;
import java.io.*;

public class TextClausesDatabase extends JIPClausesDatabase
{
    public final static String SEPARATOR = "#";
    
    private PrintWriter m_writer;
    private String      m_strFileName;
    
    public TextClausesDatabase()
    {
        super();
    }

    public String getFileName()
    {
        return m_strFileName;
    }
    
    public void setAttributes(String strAttribs)
    {
        m_strFileName = strAttribs;
    }
    
    public boolean addClauseAt(int nPos, JIPClause clause)
    {
        return false;
    }
    
    public synchronized boolean addClause(JIPClause clause)
    {
        try
        {
            m_writer = new PrintWriter(new FileWriter(m_strFileName, true));
        }
        catch(IOException ex)
        {
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 1, ex.toString());
        }
        
        String strLine = "";
        
        try
        {
            // extracts the functor
            JIPFunctor fun = clause.getHead();
            
            // get the parameters
            JIPCons params = fun.getParams();
                
            JIPCons tail = params;
            String strVal;
            
            while(tail != null)
            {
                JIPTerm head = tail.getHead();

                // if is an unbounded variable print only the name
                strVal = head.toString();
                if(head instanceof JIPVariable)
                    if (!((JIPVariable)head).isBounded())
                        strVal = ((JIPVariable)head).getName();
                
                strLine += strVal + SEPARATOR;
                tail = (JIPCons)tail.getTail();
            }
        }
        catch(ClassCastException ex)
        {
            m_writer.close();
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 4, ex.toString());
        }
                               
        m_writer.println(strLine);
        m_writer.flush();
        m_writer.close();
       
        return true;
    }
    
    public boolean removeClause(JIPClause clause)
    {
        return false;
    }
    
    public Enumeration clauses()
    {
        return new TextClausesEnumeration(this);
    }
    
    public void finalize()
    {
        m_writer.close();
    }
}
