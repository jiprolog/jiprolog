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

//import com.ugos.debug.*;

import java.util.Enumeration;
import java.util.Stack;

final class RulesEnumeration extends Object implements Enumeration<PrologRule>
{
    private final Enumeration   m_selProgramEnum;
    private boolean             m_bModuleTranparent;
    private PrologRule          m_rule;

    public RulesEnumeration(final Functor query, final Stack<String> moduleStack, final GlobalDB gdb)
    {
        final JIPClausesDatabase db = gdb.search(query, moduleStack);
        if(db == null)
            throw new UndefinedPredicateException(query);

        m_selProgramEnum  = db.clauses(query);
        m_bModuleTranparent = db.isModuleTransparent();
        m_rule = new PrologRule();
        m_rule.m_strModule = moduleStack.peek();
    }

    public final boolean hasMoreElements()
    {
        return m_selProgramEnum.hasMoreElements();
    }

    public final PrologRule nextElement()
    {
        final Clause dbCurrentClause = (Clause)m_selProgramEnum.nextElement();

        // il riuso dello stesso oggetto Rule evita la creazione di nuovi
        // oggetti che pesano sul garbage collector
        m_rule.m_cons = (Clause)dbCurrentClause.copy(true);
        m_rule.m_dbCons = dbCurrentClause;

        // controlla se module_transaprent
        if(!m_bModuleTranparent)
        {
            m_rule.m_strModule = dbCurrentClause.getModuleName();
        }

        return m_rule;
    }

	// per evitare choice-points che eseguendo un look-haed
	// non sarebbero presenti.
    // Manca però il metodo per riusare il nextElement della enumeration
//    public boolean isNextUnifiable(PrologObject obj)
//    {
//        boolean bUnify = false;
//        PrologRule rule;
//        ConsCell clause;
//        while(hasMoreElements() && !bUnify)
//        {
//            rule   = (PrologRule)nextElement();
//            clause = rule.m_cons;
//
////            System.out.println("clause " + clause);  // dbg
//            // UNIFY
//            // unifica la testa della clausola con il predicato corrente
//            bUnify = obj.unifiable(clause.getHead());
//        }
//    }
}
