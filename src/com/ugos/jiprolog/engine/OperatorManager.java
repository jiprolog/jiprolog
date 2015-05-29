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
import java.util.Hashtable;

final class OperatorManager
{
    private final Hashtable m_opTable = new Hashtable(120);

    public OperatorManager()
    {
        reset();
    }

    public final void reset()
    {
        m_opTable.clear();

//      gli opeatori definiti in jipkernel.txt
        // devono essere definiti qui.
        // il motivo mi sfugge!!!

        put(1200, "fx", ":-");
        put(1200, "fx", "?-");
        put(1200, "xfx", ":-");
        put(1200, "xfx", "-->");
        put(1100, "xfy", ";");
        put(1000, "xfy", ",");
        put(1050, "xfy", "->");
        put(1050, "xfy", "*->");
        put(901, "fy",  "{");
        put(900, "xf",  "}");
        put(900, "fy",  "\\+");
        put(900, "fy",  "not");
        put(900, "fy",  "spy");
        put(900, "fy",  "nospy");
        put(700, "xfx", "=");
        put(700, "xfx", "is");
        put(700, "xfx", "=..");
        put(700, "xfx", "\\=");
        put(700, "xfx", "==");
        put(700, "xfx", "\\==");
        put(700, "xfx", "=:=");
        put(700, "xfx", "=\\=");
        put(700, "xfx", "<");
        put(700, "xfx", ">");
        put(700, "xfx", "=<");
        put(700, "xfx", ">=");
        put(700, "xfx", "@>");
        put(700, "xfx", "@<");
        put(700, "xfx", "@=");
        put(700, "xfx", "@=<");
        put(700, "xfx", "@>=");
        put(600, "xfy", ":");
        put(500, "yfx", "+");
        put(500, "yfx", "-");
        put(500, "fx", "-");
        put(500, "fx", "+");
        put(400, "yfx", "*");
        put(400, "yfx", "/");
        put(400, "yfx", "//");
        put(200, "xfy", "^");
        put(200, "xfx", "**");

        put(500, "yfx", "/\\");
        put(500, "yfx", "\\/");
//        put(500, "fx", "\\");
        put(200, "fy", "\\");
        put(400, "yfx", "<<");
        put(400, "yfx", ">>");
        put(400, "yfx", "xor");

        put(400, "yfx", "mod");
        put(400, "yfx", "rem");

        put(1150, "fx",  "multifile");
        put(1150, "fx",  "module_transparent");
        put(1150, "fx", "dynamic");
        put(1150, "fx", "discontiguous");
        put(400, "yfx", "div");

        //put(400, "fx", "cd");
//      put(400, "xfx", "class");
//      put(400, "fx",  "class");
//      put(450, "xfx", "checks");
//      put(450, "yfx", "body");
//      put(150, "yfx", "-&-");
//      put(100, "yfx", "=>");

        /*   prova
         put(1000, "yfx", "$$");
         put(800, "yfx", "$>$");
         put(800, "xfy", "$#$");
         put(800, "xfy", "$<$");
         put(800, "xfx", "@@");

         put(600, "fx", "$");
         put(600, "xf", "#");
         put(600, "yf", "@");
         */
    }
    public final void put(final int nPrec, final String strAssoc, final String strName)
    {
        Operator newOp = new Operator(nPrec, strAssoc, strName);
        if(m_opTable.containsKey(strName))
        {
            Operator op = (Operator)m_opTable.get(strName);

            if((op.isBinary() && newOp.isUnary()) || (op.isUnary() && newOp.isBinary()))
            {
                // sta aggiungendo il supOP
                op.m_suppOp = newOp;
                newOp.m_suppOp = op;
                return;
            }
            else
            {
                // sta aggiungendo nuovamente l'operatore stesso
                // quindi aggiorna solo gli attributi
                op.m_strAssoc = strAssoc;
                op.m_nPrecedence = nPrec;
            }
        }
        else
        {
	//        System.out.println("put OP" + strName);
	        m_opTable.put(strName, newOp);
	//        System.out.println(o);
        }
    }

    public final Operator get(final String strName)
    {
        return (Operator)m_opTable.get(strName);
    }

    public final void remove(final String strAssoc, final String strName)
    {
        //System.out.println("remove op:" + strName + " " + strAssoc);
        if(m_opTable.containsKey(strName))
        {
            Operator op = (Operator)m_opTable.get(strName);

            if(op.getAssoc().equals(strAssoc))
            {
                //System.out.println("coincide");
                // se coincide si deve gestire il supOp
                if(op.m_suppOp != null)
                {
                    //System.out.println("supOP");
                    // se l'operatore corrente ha un supOp lo sostituisce nella tabella
                    op.m_suppOp.m_suppOp = null;
                    m_opTable.put(strName, op.m_suppOp);
                }
                else
                {
                    //System.out.println("No supOP");
                    // altrimenti rimuove l'operatore corrente
                    m_opTable.remove(strName);
                }
            }
            else if(op.m_suppOp != null)
            {
                //System.out.println("non coincide");
                // controlla il suoop
                if(op.m_suppOp.getAssoc().equals(strAssoc))
                {
                    //System.out.println("supOP2");
                    // rimuove solo il supOp
                    op.m_suppOp = null;
                }
            }
        }
    }

    public final boolean contains(final String strName)
    {
        return m_opTable.containsKey(strName);
    }

    public final Enumeration getOperators()
    {
        return m_opTable.elements();
    }
}
