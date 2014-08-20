/*
 * 09/19/2002
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

package com.ugos.JIProlog.extensions.database;

import com.ugos.JIProlog.engine.*;
import java.util.*;
import java.io.*;
import java.util.StringTokenizer;
import java.sql.*;

public class JDBCClausesEnumeration extends JIPClausesEnumeration
{
    private int 			rowIndex;
    private List<JIPClause> clauseList;
    
    public JDBCClausesEnumeration(JDBCClausesDatabase db)
    {
        super(db);
        rowIndex = 0;
        
    	clauseList = db.getClauses();
    }
    
    public final boolean hasMoreElements()
    {
    	if(rowIndex >= clauseList.size())
    	{
    		int n;
			try 
			{
				n = ((JDBCClausesDatabase)getDatabase()).readNextRow(10);
			} 
			catch (SQLException e) 
			{
				finalize();
	            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 6, e.toString());
			}
			
    		if(n == 0)
    			return false;
    		else    
    			clauseList = ((JDBCClausesDatabase)getDatabase()).getClauses();
    	}
    	
		return true;    	
    }
    
    
    public final JIPClause nextClause()
    {
    	if(rowIndex >= clauseList.size())
    		throw new NoSuchElementException("No more clauses available in JDBCClausesDatabase");
    	
    	JIPClause clause = clauseList.get(rowIndex);
    	
    	rowIndex++;
    	
    	return clause;   	
    }
    
        
    protected void finalize()
    {
    	
    }
}
