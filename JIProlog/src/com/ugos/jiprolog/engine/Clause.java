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

import java.util.Hashtable;
import java.io.*;

class Clause extends ConsCell
{
    final static long serialVersionUID = 300000002L;
    
    //    private String m_strFileName;
    private String  m_strModuleName;
    private boolean m_bExported = false;
    private String  m_strFileName = null;//"none";
    private int     m_nPosition = 0;
    private int     m_nLineNumber = 0;
    
    private static JIPEngine s_engine = null;
    private static Functor s_translateQuery = null;
    private static ConsCell s_translateParams = null;
    
    
//  Clause(Clause master)
//    {
//      this(master.m_strModuleName, (Functor)master.getHead(), (ConsCell)master.getTail());
//      m_bExported = master.m_bExported;
//    }

    Clause(String strModuleName, final Functor lhs, final ConsCell rhs)
    {
        super(lhs, rhs);
        m_strModuleName = strModuleName;
    }
    
    private Clause(final ConsCell cell, String strModuleName)
    {
        this(strModuleName, (Functor)cell.m_head, (ConsCell)cell.m_tail);
    }
    
    final void setModuleName(final String strModuleName)
    {
        m_strModuleName = strModuleName;
    }
    
    final String getModuleName()
    {
        return m_strModuleName;
    }
    
    final void setExported()
    {
        m_bExported = true;
    }
    
    final void setFileName(String strFileName)
    {
        m_strFileName = strFileName;
    }
    
    final void setPosition(int nPos)
    {
        m_nPosition = nPos;
    }
    
    final void setLineNumber(int nLineNumber)
    {
        m_nLineNumber = nLineNumber;
    }
    
    final String getFileName()
    {
        return m_strFileName;
    }
    
    final int getPosition()
    {
        return m_nPosition;
    }
    
    final int getLineNumber()
    {
        return m_nLineNumber;
    }
    
    final boolean isExported()
    {
        return m_bExported;
    }

    public final PrologObject copy(final Hashtable<Variable, Variable> varTable)
    {
        final Clause clause = new Clause((ConsCell)super.copy(varTable), m_strModuleName);
        clause.m_bExported = m_bExported;
        clause.m_strFileName = m_strFileName;
        clause.m_nLineNumber = m_nLineNumber;
        clause.m_nPosition = m_nPosition;
        
        return clause;
    }
    
    static final Clause getClause(PrologObject pred)
    {
        return getClause(pred, GlobalDB.USER_MODULE);
    }

    static final Clause getClause(PrologObject pred, String strModuleName)
    {
//      System.out.println("getClause");
//      System.out.println(pred.getClass());
//      System.out.println(pred);
                
        if(pred instanceof Variable)
            pred = ((Variable)pred).getObject();
        
        if(pred instanceof Clause)
            return (Clause)pred;
        
        if(pred instanceof Atom)
            pred = new Functor(((Atom)pred).getName() + "/0", null);
        
        if(!(pred instanceof Functor))
            throw new JIPParameterTypeException(1, JIPParameterTypeException.FUNCTOR);
                    
        Functor func = (Functor)pred;
        
        Clause clause;
        ConsCell params;
        //String strModuleName = GlobalDB.USER_MODULE;
        
        if(func.getName().equals(":-/2"))
        {
            // estrae la clausola
            params = func.getParams();
            
            PrologObject lhs = BuiltIn.getRealTerm(params.getHead());
            //PrologObject rhs = BuiltIn.getRealTerm(((ConsCell)params.getTail()).getHead());
            PrologObject rhs = BuiltIn.getRealTerm(params.getTail());
            
//            System.out.println("lhs: " + lhs);
//            System.out.println("rhs: " + rhs);
            
            // verifica se lhs ha la specifica del modulo
            if((lhs instanceof Functor) && ((Functor)lhs).getName().equals(":/2"))
            {
                strModuleName = ((Atom)((Functor)lhs).getParams().getHead()).getName();
                //lhs = BuiltIn.getRealTerm((((Functor)lhs).getParams().getTail()));
                lhs = BuiltIn.getRealTerm(((ConsCell)((Functor)lhs).getParams().getTail()).getHead());
//                System.out.println("lhs: " + lhs);
//                System.out.println("lhs: " + lhs.getClass());
            }
            
            if(lhs instanceof Atom)
            {
                lhs = new Functor(((Atom)lhs).getName() + "/0", null);
            }
            
//          if(rhs instanceof List || rhs instanceof Functor || !(rhs instanceof ConsCell))
//              rhs = new ConsCell(rhs, null);
                                    
//            System.out.println("lhs: " + lhs);
//            System.out.println("rhs: " + rhs);
            
            clause = new Clause(strModuleName, (Functor)lhs, (ConsCell)rhs);
        }
        else if(func.getName().equals("-->/2"))
        {
            PrologObject translated;
            // chiama il prolog per la translation
            if(s_engine == null)
                s_engine = new JIPEngine();
            
            if(s_translateQuery == null)
            {
                final PrologParser parser = new PrologParser(new ParserReader(new InputStreamReader(new ByteArrayInputStream("translate(X, Y)".getBytes()))), new OperatorManager(),"internal");
                try
                {
                    final Functor funct = ((Functor)parser.parseNext());
                    s_translateParams = funct.getParams();
                    s_translateQuery = new Functor(":/2", new ConsCell(Atom.createAtom(GlobalDB.KERNEL_MODULE), new ConsCell(funct, null)));
                }
                catch(JIPSyntaxErrorException ex)
                {
                    throw new JIPRuntimeException(ex.toString());
                }
            }
            Variable vTranslated = new Variable("Y");
            s_translateParams.setHead(func);
            ((ConsCell)s_translateParams.getTail()).setHead(vTranslated);
            
//            System.out.println("s_translateQuery " + s_translateQuery);
            WAM wam = new WAM(s_engine);
                        
            if(wam.query(new ConsCell(s_translateQuery, null)))
            {
                wam.closeQuery();
    //              System.out.println("vPredList " + vPredList);
                // estrae la collection di clausole
                translated = BuiltIn.getRealTerm(vTranslated);
            
                // chiama getClause e ritorna
                clause = getClause(translated.copy(), strModuleName);
                
                wam.closeQuery();
                
                return clause;
            }
            else
            {
                throw new JIPParameterTypeException(1, JIPParameterTypeException.CALLABLE);
                //throw new JIPRuntimeException("unable to translate clause!!!!");
            }
        }
        else if(func.getName().equals(":/2"))
        {
            // solo funtore con specifica di modulo
            // il body è vuoto
            strModuleName = ((Atom)(func).getParams().getHead()).getName();
            PrologObject lhs = BuiltIn.getRealTerm(((ConsCell)(func).getParams().getTail()).getHead());
            
            if(lhs instanceof Atom)
            {
                lhs = new Functor(((Atom)lhs).getName() + "/0", null);
            }
                        
            //lhs = new Functor(strModuleName + ":" + ((Functor)lhs).getName(), ((Functor)lhs).getParams());
                            
            clause = new Clause(strModuleName, (Functor)lhs, null);
            
        }
        else
        {
            // solo funtore in modulo user
            //func = new Functor(strModuleName + ":" + ((Functor)func).getName(), ((Functor)func).getParams());
                            
            clause = new Clause(strModuleName, func, null);
            
        }
        
//        clause.setModuleName(strModuleName);
//        System.out.println("clause: " + clause );
//        System.out.println("module: " + clause.getModuleName() );
//
        return clause;
    }
}
