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

import java.util.*;

/**
 * JIPClausesDatabase is the base class for external database of clauses<br>
 * To implement a custom database of clauses, developers should implement the following methods:<br>
 * - setAttributes<br>
 * - addClauseAt<br>
 * - addClause<br>
 * - removeClause<br>
 * - clauses<br>
 * For more information see the section <i>"How to implement a custom detabase of clauses"</i> in the Reference Manual.
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPClausesEnumeration
 */
public abstract class JIPClausesDatabase extends Object
{
    private String      m_strFuncName;
    private int         m_nArity;
    private int         m_nIndex = 1; // default for first argument indexing

    private Hashtable<String, JIPClausesDatabase> m_propTbl;

    private JIPEngine   m_jip;

    /** Constucts a new JIPClausesDatabase
     */
    public JIPClausesDatabase()
    {
        m_propTbl = new Hashtable<String, JIPClausesDatabase>(4);
    }

    /** Returns the name of the functor associated to this database
     */
    public final String getFunctorName()
    {
        return m_strFuncName;
    }

    /** Returns the arity of the functor associated to this database
      */
    public final int getArity()
    {
        return m_nArity;
    }

    /** Returns the JIPEngine object attached to this database
      */
    public final JIPEngine getJIPEngine()
    {
        return m_jip;
    }

    /** Sets the index for argument indexing. By default it is 1 for first argument indexing
     */
   public void setIndex(int index)
   {
	   if(index < 1 || index > m_nArity)
		   throw new JIPDomainException("integer", JIPNumber.create(index));

	   this.m_nIndex = index;
   }

   /** Gets the index for argument indexing. By default it is 1 for first argument indexing
    */
   public final int getIndex()
   {
	   return this.m_nIndex;
   }

    /** Sets the attributes to pass to the database (i.e. login info, filename, etc.)
      * @param strAttribs the attributes to pass
      */
    public abstract void setAttributes(final String strAttribs);

    /** Adds a clause to the database at the first position<br>
      * Note that some databases may not allow to add a clause at the first position.<br>
      * @param clause Clause to add
      * @return true if the clause has been added.
      * @see com.ugos.jiprolog.engine.JIPClause
      */
    public abstract boolean addClauseAtFirst(final JIPClause clause);

    /** Appends a clause to the database<br>
     * Note that some databases may not allow to append a clause.<br>
     * @param clause Clause to add
     * @return true if the clause has been appended.
     * @see com.ugos.jiprolog.engine.JIPClause
     */
    public abstract boolean addClause(final JIPClause clause);

    /** Removes a clause from the database
     * Note that some databases may not allow to remove a clause.<br>
     * @param clause Clause to remove
     * @return true if the clause has been removed.
     * @see com.ugos.jiprolog.engine.JIPClause
     */
    public abstract boolean removeClause(final JIPClause clause);

    /** Returns an enumeration of the clauses contained in this database that matches the functor using the argument indexing on the index set by setIndex
     * @see com.ugos.jiprolog.engine.JIPClausesDatabase#setIndex(int)
     * @see com.ugos.jiprolog.engine.JIPClause
     * @see com.ugos.jiprolog.engine.JIPClausesEnumeration
     * @return an enumeration of the clauses contained in this database
     */
    public abstract Enumeration<JIPClause> clauses(JIPFunctor functor);

    /** Returns an enumeration of the all clauses contained in this database
     * @see com.ugos.jiprolog.engine.JIPClause
     * @see com.ugos.jiprolog.engine.JIPClausesEnumeration
     * @return an enumeration of the clauses contained in this database
     */
    public abstract Enumeration<JIPClause> clauses();

    Enumeration<JIPClause> clauses(Functor functor)
    {
    	return clauses(new JIPFunctor(functor));
    }

    final void setFunctor(final String strFuncName, final int nArity)
    {
        m_strFuncName   = strFuncName;
        m_nArity        = nArity;
//      m_strModuleName = strModuleName;
    }

    final void setJIPEngine(final JIPEngine jip)
    {
        m_jip = jip;
    }

    final void setModuleTransparent()
    {
        m_propTbl.put("mt", this);
        //m_bModuleTransparent = true;
    }

    final boolean isModuleTransparent()
    {
        return m_propTbl.containsKey("mt");//m_bModuleTransparent;
    }

    final void setMultifile()
    {
        m_propTbl.put("mf", this);
        //m_bMultifile = true;
    }

    final boolean isMultifile()
    {
        return m_propTbl.containsKey("mf");
        //return m_bMultifile;
    }

    final void setDynamic()
    {
        m_propTbl.put("dy", this);
        //m_bMultifile = true;
    }

    final boolean isDynamic()
    {
        return m_propTbl.containsKey("dy");
        //return m_bMultifile;
    }

    final void setExternal()
    {
        m_propTbl.put("ext", this);
    }

    final boolean isExternal()
    {
        return m_propTbl.containsKey("ext");
    }

}
