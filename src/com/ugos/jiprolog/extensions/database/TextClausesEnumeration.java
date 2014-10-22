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

package com.ugos.jiprolog.extensions.database;

import com.ugos.jiprolog.engine.*;

import java.util.Enumeration;
import java.io.*;
import java.util.StringTokenizer;

public class TextClausesEnumeration extends JIPClausesEnumeration
{
    private LineNumberReader m_reader;
    private JIPClause        m_curClause;
    private boolean          m_bUsed;
    
    public TextClausesEnumeration(TextClausesDatabase db)
    {
        super(db);
        
        try
        {
            m_reader = new LineNumberReader(new FileReader(db.getFileName()));
        }
        catch(IOException ex)
        {
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 1, ex.toString());
        }
                      
        updateCurClause();
    }
     
    public boolean hasMoreElements()
    {
        if(m_bUsed)
            updateCurClause();
               
        if(m_curClause == null)
        {
            finalize();
            return false;
        }
            
        return true;
    }
    
    public JIPClause nextClause()
    {
        m_bUsed = true;
        return m_curClause;
    }
    
    private void updateCurClause()
    {
        m_bUsed = false;
        
        String strLine;
        try
        {
            // read next line
            strLine = m_reader.readLine();

            if(strLine == null)
            {
                m_curClause = null;
                m_reader.close();
                return;
            }
        }
        catch(IOException ex)
        {
            throw new JIPRuntimeException(101, ex.toString());
        }
     
        //separate parameters by StringTokenizer
        StringTokenizer stk = new StringTokenizer(strLine, "#");
        
        String  strTerm;
        JIPTerm term;
        JIPCons list = null;
        
        int i = 0;
        while (stk.hasMoreTokens())
        {
            i++;
            
            // extract next term
            strTerm = stk.nextToken();

            try
            {
                //parse the term extracted
                term    = getDatabase().getJIPEngine().getTermParser().parseTerm(strTerm);
            }
            catch(JIPSyntaxErrorException ex)
            {
                finalize();
                throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 2, ex.toString());
            }
            
            //add to list
            list = JIPCons.create(term, list);
        }
        
        if(i != getDatabase().getArity())
        {
            finalize();
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 3, "The arity of the extern predicate doesn't match with the expected one");
        }

        // reverse the list because was constructed from tail
        list = list.reverse();
        
        // make functor
        JIPFunctor func = JIPFunctor.create(getDatabase().getFunctorName(), list);
        
        // make clause
        m_curClause = JIPClause.create(func, null);
    }
       
    public void finalize()
    {
        try
        {
            m_reader.close();
        }
        catch(IOException ex){};
    }
}
