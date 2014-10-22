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

package com.ugos.jiprolog.engine;

import java.util.Enumeration;

final class RulesEnumerationBuiltIn extends Object implements Enumeration
{
//    private boolean  m_bHasMoreChoicePoints;
    private Clause   m_currentClause;
    private final String           m_strModule;
    private final BuiltInPredicate m_query;
    private PrologRule             m_rule;
            
    public RulesEnumerationBuiltIn(final BuiltInPredicate query, final String strModule, final WAM wam)
    {
        m_query = query;
               
        // all'interno della stessa enumeration vive una unica instanza
        // del builtin che muore quando l'enumeration viene rilasciata
        m_query.init(wam);
        m_currentClause = new Clause(GlobalDB.SYSTEM_MODULE, m_query, null);
        m_strModule = strModule;
        
        // la prima volta non e' deterministico
//        m_bHasMoreChoicePoints = true;
        m_rule = new PrologRule();
    }
    
    public final boolean hasMoreElements()
    {
        if(m_rule.m_cons == null)  // mai chiamata
            return true;
        
        return m_query.hasMoreChoicePoints();
//        if(!m_bHasMoreChoicePoints)
//            return false;
//
//        m_bHasMoreChoicePoints = m_query.hasMoreChoicePoints();
//
//        return m_bHasMoreChoicePoints;
    }

    public final Object nextElement()
    {
        // il riuso dello stesso oggetto Rule evita la creazione di nuovi
        // oggetti che pesano sul garbage collector
        m_rule.m_cons = m_currentClause;
        m_rule.m_strModule = m_strModule;
        m_rule.m_dbCons = m_currentClause;
                
        return m_rule;
    }
}
