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

/*
 example:
    extern(tbl/3, "com.ugos.jiprolog.extensions.xdb.JDBCClausesDatabase", "sun.jdbc.odbc.JdbcOdbcDriver,jdbc:odbc:prolog,table:Tabella").
 */

package com.ugos.jiprolog.extensions.database;

import com.ugos.jiprolog.engine.*;

import java.util.*;
import java.sql.*;

public class JDBCClausesDatabase extends JIPClausesDatabase
{
    public final static String SEPARATOR = ",";
    public final static String QUOTE = "'";

    private String      m_strTableName  = null;
    private String      m_strSQLQuery   = null;
    private String      m_strURL        = null;
    private String      m_strUser        = null;
    private String      m_strPassword        = null;

    private ResultSetMetaData m_rsmd       = null;
    private Statement         m_stmt       = null;
    private ResultSet         m_rs         = null;

    private List<JIPClause> clauseList;


    private static Hashtable<String, Connection> s_connectionTable = new Hashtable<String, Connection>();

    public JDBCClausesDatabase()
    {
    	clauseList = new ArrayList<JIPClause>();
    }

    public final Connection getConnection()
    {
        return s_connectionTable.get(m_strURL);
    }

    public final String getURL()
    {
        return m_strURL;
    }

    public final String getSQLQuery()
    {
        if(m_strTableName != null)
            return "SELECT * FROM " + m_strTableName;
        else if(m_strSQLQuery != null)
            return m_strSQLQuery;
        else
            return null;
    }

    public final void setAttributes(String strAttribs)
    {
        // Driver Name
        int nBegin = 0;
        int nEnd = strAttribs.indexOf(',');
        if(nBegin > nEnd)
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 8, "Wrong number or attributes");

        String strDriverName = strAttribs.substring(nBegin, nEnd);
        strDriverName = strDriverName.trim();
//        System.out.println(strDriverName);  // DBG

        // Url
        nBegin = nEnd + 1;
        nEnd = strAttribs.indexOf(',', nBegin);
        if(nBegin > nEnd)
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 8, "Wrong number or attributes");

        m_strURL = strAttribs.substring(nBegin, nEnd);
        m_strURL = m_strURL.trim();

//        System.out.println(m_strURL);  // DBG

        // Table Name or SQL
        nBegin = nEnd + 1;
        nEnd = strAttribs.indexOf('@', nBegin);

        if(nEnd == -1)
            nEnd = strAttribs.length();

        String strQuery = strAttribs.substring(nBegin, nEnd);
        strQuery = strQuery.trim();

        nBegin = strQuery.indexOf(':');

        if(nBegin > nEnd || nBegin < 0)
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 11, "Invalid Table or Query specification, missing ':'");

        nBegin++;


//        System.out.println(strQuery);  // DBG

        if(strQuery.toUpperCase().startsWith("TABLE:"))
        {
            m_strTableName = strQuery.substring(nBegin, strQuery.length());
        }
        else if(strQuery.toUpperCase().startsWith("SQL:"))
        {
            m_strSQLQuery = strQuery.substring(nBegin, strQuery.length());
            if(m_strSQLQuery.toUpperCase().indexOf("SELECT") < 0)
                throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 12, "The SQL specified does not seem a SELECT query: " + m_strSQLQuery);
        }
        else
        {
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 11, "Invalid Table or Query specification: " + strQuery);
        }

        //UserName and Password
        nBegin = nEnd + 1;
        nEnd = strAttribs.indexOf(',', nBegin);
        if(nBegin < nEnd)
        {
            m_strUser = strAttribs.substring(nBegin, nEnd);
            nBegin = nEnd + 1;
            if(nBegin < strAttribs.length())
            {
                m_strPassword = strAttribs.substring(nBegin, strAttribs.length());
            }
            else
            {
            	m_strPassword = ""; // empty
            }
        }

        //System.out.println(m_strTableName);

        //////////////////////////////////////////////
        // Load driver
        try
        {
            // loaded by this class's classloader
            getClass().forName(strDriverName);
        }
        catch(ClassNotFoundException ex)
        {
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 9, "Unable to load the JDBC driver specified");
        }

        // Establish connection.
        Connection connection;
        try
        {
            if(!s_connectionTable.containsKey(m_strURL))
            {
                if(m_strUser == null)
                    connection = DriverManager.getConnection(m_strURL);
                else
                    connection = DriverManager.getConnection(m_strURL, m_strUser, m_strPassword);

                s_connectionTable.put(m_strURL, connection);
            }
            else
            {
                connection = (Connection)s_connectionTable.get(m_strURL);
            }
        }
        catch (SQLException e)
        {
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 6, e.getMessage());
        }

        // Read table to check arity
//        try
//        {
//            m_stmt = connection.createStatement();
//            String query = getSQLQuery();
//            if(!m_stmt.execute(query))
//            {
//            	finalize();
//
//            	// Check Arity
//                if(getArity() != 0)
//                {
//                    throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 10, "The arity doesn't match with the number of columns in the table or query");
//                }
//
//                return;//throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 13, "The query specified doesn't return a valid result set: " + m_strSQLQuery);
//            }
//
//            ResultSet rs = m_stmt.getResultSet();
//            m_rsmd = rs.getMetaData();
//
//            // Check Arity
//            if(getArity() != m_rsmd.getColumnCount())
//            {
//                finalize();
//                throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 10, "The arity doesn't match with the number of columns in the table or query");
//            }
//        }
//        catch (SQLException e)
//        {
//            finalize();
//            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 6, e.getMessage());
//        }
    }

    public final boolean addClauseAtFirst(JIPClause clause)
    {
        return false;
    }

    public final synchronized boolean addClause(JIPClause clause)
    {
        // If it is attached to a view via SQL query it cannot add a clause
        if(m_strSQLQuery != null)
            return false;

        // It is attached directly to a table

        String strCol = null;
        // Prepare row from clause
        try
        {
            // extracts the functor
            JIPFunctor fun = clause.getHead();

            // get the parameters
            JIPCons params = fun.getParams();

            JIPCons tail = params;
            String strVal = null;
            int i = 0;
            while(tail != null)
            {
                i++;

                if(strCol == null)
                    strCol = "";
                else
                    strCol += SEPARATOR;

                JIPTerm head = tail.getHead();

                if(!JDBCClausesDatabase.isValid(m_rsmd.getColumnType(i)))
                {
                    throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 7, "SQL type of the column n. " + (i) + " cannot be interpreted in prolog");
                }

                // if is an unbounded variable print only the name
                if(head instanceof JIPVariable)
                {
                    if (!((JIPVariable)head).isBounded())
                        strVal = ((JIPVariable)head).getName();
                }
                else if(head instanceof JIPAtom)
                {
                    strVal = head.toString();
                    //double '
                    int nPos = 0;
                    int nBegin = 0;
                    while(nPos > -1)
                    {
                        nPos = strVal.indexOf('\'', nBegin);
                        if(nPos != -1)
                        {
                            strVal = strVal.substring(0, nPos) + '\'' + strVal.substring(nPos, strVal.length());
                            nBegin = nPos + 2;
                        }
                    }
                }
                else
                {
                    strVal = head.toString();
                }

                if(JDBCClausesDatabase.isString(m_rsmd.getColumnType(i)))
                {
                    strVal = "'" + strVal + "'";
                }

                strCol += strVal;
                tail = (JIPCons)tail.getTail();
            }

            // Execute INSERT query
            Statement stmt = getConnection().createStatement();

            String strSQL =
                "INSERT INTO " + m_strTableName + " VALUES ( " + strCol + " )";

//          System.out.println(strSQL);

            stmt.executeUpdate(strSQL);

            stmt.close();
        }
        catch(ClassCastException ex)
        {
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 4, ex.toString());
        }
        catch(SQLException ex)
        {
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 6, ex.toString());
        }

        return true;
    }

    public final boolean removeClause(JIPClause clause)
    {
        // If it is attached to a view via SQL query it cannot remove clauses
        if(m_strSQLQuery != null)
            return false;

        // Get the matching row
        JDBCClausesEnumeration en = (JDBCClausesEnumeration)clauses(clause.getHead());
        boolean bMatch = false;
        JIPClause nextClause = null;

        while(en.hasMoreElements() && !bMatch)
        {
            nextClause = en.nextClause();
            bMatch = clause.unifiable(nextClause);
        }

        if(!bMatch)
            return false;

        // Prepare WHERE statement
        String strWhere = null;
        String strColName;

        // extract the functor
        JIPFunctor fun = nextClause.getHead();
        // get the parameters
        JIPCons params = fun.getParams();
        JIPCons tail = params;

        try
        {
            int i = 1;
            while(tail != null)
            {
                String strVal = null;
                strColName = m_rsmd.getColumnName(i);
                JIPTerm head = tail.getHead();
                // if is an unbounded variable DON'T ADD IT because it always match
                if(head instanceof JIPVariable)
                {
                    if (!((JIPVariable)head).isBounded())
                        strVal = ((JIPVariable)head).getName();
                    else
                        strVal = null;
                }
                else
                {
                    strVal = head.toString();
                }

                if(strVal != null)
                {
                    if(!JDBCClausesDatabase.isValid(m_rsmd.getColumnType(i)))
                    {
                        throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 7, "SQL type of the column n. " + (i) + " cannot be interpreted in prolog");
                    }

                    if(JDBCClausesDatabase.isString(m_rsmd.getColumnType(i)))
                    {
                        strVal = "'" + strVal + "'";
                    }

                    if(strWhere != null)
                        strWhere += " AND " + strColName + " = " + strVal;
                    else
                        strWhere = strColName + " = " + strVal;
                }

                tail = (JIPCons)tail.getTail();
                i++;
            }

            Statement stmt = getConnection().createStatement();
            stmt.execute("DELETE ONCE FROM " + m_strTableName
                             + " WHERE " + strWhere);
            stmt.close();
        }
        catch(SQLException ex)
        {
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 6, ex.toString());
        }

        return true;
    }

    public final Enumeration clauses(JIPFunctor functor)
    {
    	Connection connection = getConnection();
    	try
    	{
			if(connection.isClosed())
			{
				if(m_strUser == null)
			        connection = DriverManager.getConnection(m_strURL);
			    else
			        connection = DriverManager.getConnection(m_strURL, m_strUser, m_strPassword);

			    s_connectionTable.put(m_strURL, connection);
			}
		}
    	catch (SQLException e)
    	{
    		 throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 6, e.getMessage());
		}

    	return new JDBCClausesEnumeration(this);
    }

	@Override
	public Enumeration clauses() {

		return clauses(null);
	}
    public ResultSet getResultSet() throws SQLException
    {
    	if(m_rs == null)
    	{
    		Statement stmt = getConnection().createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
    		String query = getSQLQuery();
		    if(stmt.execute(query))
		    {
		        m_rs = stmt.getResultSet();
		        m_rsmd = m_rs.getMetaData();
                // Check Arity
                if(getArity() != m_rsmd.getColumnCount())
                {
                    finalize();
                    throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 10, "The arity doesn't match with the number of columns in the table or query");
                }
		    }
		    else
		    {
            	finalize();

            	// Check Arity
                if(getArity() != 0)
                {
                    throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 10, "The arity doesn't match with the number of columns in the table or query");
                }
		    }
    	}

    	return m_rs;
    }

    public void finalize()
    {
        try
        {
            if(m_rs != null)
            	m_rs.close();

            if(m_stmt != null)
                m_stmt.close();
        }
        catch(SQLException ex){}
    }

    public static final boolean isNumber(int nType)
    {
        return  nType == Types.BIGINT ||
                nType == Types.BIT    ||
                nType == Types.DECIMAL ||
                nType == Types.DOUBLE  ||
                nType == Types.FLOAT  ||
                nType == Types.INTEGER  ||
                nType == Types.NUMERIC  ||
                nType == Types.REAL  ||
                nType == Types.SMALLINT  ||
                nType == Types.TINYINT;
    }

    public static final boolean isValid(int nType)
    {
        return !(nType == Types.BINARY ||
                 nType == Types.LONGVARBINARY ||
                 nType == Types.OTHER ||
                 nType == Types.VARBINARY);// ||
                 //nType == Types.NULL);
    }

    public static final boolean isString(int nType)
    {
        return  nType == Types.CHAR         ||
                nType == Types.DATE         ||
                nType == Types.LONGVARCHAR  ||
                nType == Types.TIME         ||
                nType == Types.TIMESTAMP    ||
                nType == Types.VARCHAR;
    }

    public int readNextRow(int count) throws SQLException
    {
    	getResultSet();
    	if(m_rs == null)
    		return 0;

    	int i = 0;
    	for(i = 0; i < count && m_rs.next(); i++)
    	{
    		clauseList.add(getCurClause());
    	}

    	return i;
    }

    private final JIPClause getCurClause()
    {
        JIPCons list = null;

        String strCol = "";
        JIPTerm term;

        try
        {
            // Read columns
            int nCount = getArity();

            for(int i = 0; i < nCount; i++)
            {
                // read column
                strCol = m_rs.getString(i + 1);
                if(strCol == null) // NULL value
                {
                	term = JIPList.NIL;
                }
                else
                {
                    if(!JDBCClausesDatabase.isValid(m_rs.getMetaData().getColumnType(i + 1)))
                    {
                        finalize();
                        throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 7, "SQL type of the column n. " + (i + 1) + " cannot be interpreted in prolog");
                    }

                    // double "
                    int nPos = 0;
                    int nBegin = 0;

                    while(nPos > -1)
                    {
                        nPos = strCol.indexOf('\'', nBegin);
                        if(nPos != -1)
                        {
                            strCol = strCol.substring(0, nPos) + '\'' + strCol.substring(nPos, strCol.length());
                            nBegin = nPos + 2;
                        }
                    }

                    if(JDBCClausesDatabase.isString(m_rs.getMetaData().getColumnType(i + 1)))
                    {
                        strCol = JDBCClausesDatabase.QUOTE + strCol + JDBCClausesDatabase.QUOTE;
                    }

                    try
                    {
                        //parse the term extracted from the column
                        term = getJIPEngine().getTermParser().parseTerm(strCol);
                    }
                    catch(JIPSyntaxErrorException ex)
                    {
                        finalize();
                        throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 2, ex.toString());
                    }
                }

                //add to list
                list = JIPCons.create(term, list);
            }
        }
        catch(SQLException ex)
        {
            finalize();
            throw new JIPRuntimeException(JIPRuntimeException.ID_USER_EXCEPTION + 6, ex.toString());
        }

        // reverse the list because it was constructed from tail
        if(list != null)
            list = list.reverse();

        // make functor
        JIPFunctor func = JIPFunctor.create(getFunctorName(), list);

        // make clause
        return JIPClause.create(func, null);
    }

	/**
	 * @return the clauseList
	 */
	public List<JIPClause> getClauses() {
		return clauseList;
	}


}
