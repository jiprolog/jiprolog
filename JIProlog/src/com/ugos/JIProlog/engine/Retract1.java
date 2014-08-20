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

package com.ugos.JIProlog.engine;

import java.util.*;

final class Retract1 extends BuiltIn
{
    private boolean m_bEnd = false;
    
    public final boolean unify(final Hashtable varsTbl)
    {
//      System.out.println("retract");
        Clause clause = Clause.getClause(getParam(1));
        if(clause.getModuleName().equals(GlobalDB.USER_MODULE))
           clause.setModuleName(getWAM().m_curNode.m_strModule);
        
//      System.out.println(m_clause);
        Clause retractedClause = getJIPEngine().getGlobalDB().retract(clause);
        
//        System.out.println(retractedClause);
        if(retractedClause == null)
        {
            m_bEnd = true;
            return false;
        }
                
        return clause.unify(retractedClause, varsTbl);
    }
                
    public final boolean hasMoreChoicePoints()
    {
        return !m_bEnd;
    }
}
