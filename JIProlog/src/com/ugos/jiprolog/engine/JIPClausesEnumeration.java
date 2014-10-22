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

import java.awt.Frame;

import java.util.Enumeration;



/**
 * JIPClausesEnumeration is the base class to implement an enumeration of clauses contained in a custom database<br>
 * Developers should implements the methods: <br>
 * - nextClause<br>
 * - hasMoreElements<br>
 * For more information see the section <i>"How to implement a custom database of clauses"</i> in the Reference Manual.
 * @version 2.0
 * @author Ugo Chirico 2002<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.jiprolog.engine.JIPClausesDatabase
 */
public abstract class JIPClausesEnumeration implements Enumeration
{
    //private final String m_strFuncName;
    //private final int    m_nArity;
    private JIPClausesDatabase m_db;
    /** Constructs a clauses enumeration from an instance of JIPClausesDatabase
     * @param db Tha JIPClausesDatabase related to this enumeration
     * @see com.ugos.jiprolog.engine.JIPClausesDatabase
      */
    public JIPClausesEnumeration(final JIPClausesDatabase db)
    {
        m_db = db;
        //m_nArity      = db.getArity();
        //m_strFuncName = db.getFunctorName();
        // eventualmente mostra finestra di shareware
        
        //#ifndef _MIDP
//        if(!"PRO".equals(JIPEngine.getLicenseType()))
//        {
//            qwerty();
//        }
        //#endif
    }
    
    /** Gets the next clause in this enumeration. <br>
     * @return the next clause in the enumeration.
     * @see com.ugos.jiprolog.engine.JIPClause
     */
    public abstract JIPClause nextClause();
    
    /** Gets the next element of the enumeration. <br>
     * It is called by internal ASM and should not be called by developers.
     */
    public final Object nextElement()
    {
//        if(!"PRO".equals(JIPEngine.getLicenseType()))
//            JIPEngine.qwerty();
        
        return nextClause().getTerm();
    }
    
//    /** Gets the name of the functor associated to this enumeration
//      */
//    public final String getFunctorName()
//    {
//        return m_db.getFunctorName();
//    }
//
//    /** Gets the arity of the functor associated to this enumeration
//      */
//    public final int getArity()
//    {
//        return m_db.getArity();
//    }
    
    /** Gets the database associated to this enumeration
      */
    public final JIPClausesDatabase getDatabase()
    {
        return m_db;
    }
}
