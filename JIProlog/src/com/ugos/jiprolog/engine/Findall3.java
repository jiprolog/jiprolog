package com.ugos.jiprolog.engine;

import java.util.Hashtable;
import java.util.Stack;

final class Findall3 extends BuiltIn
{
    WAM m_wam;
    PrologObject m_res;
    List         m_solList;
    
    public final boolean unify(final Hashtable varsTbl)
    {
        PrologObject term  = getParam(1);
        PrologObject query = getRealTerm(getParam(2));
                    
        if(query == null)
            throw new JIPParameterUnboundedException(2);
            
        m_res = getParam(3);
        
        m_wam = getNewWAM();
        
        Stack solStack = new Stack();
        
        if(m_wam.query(new ConsCell(query, null)))
        {
            solStack.push(term.copy());
            while(m_wam.nextSolution())
            {
                solStack.push(term.copy());
            }
        }

        m_solList = null;
        
        while(!solStack.isEmpty())
        {
            m_solList = new List((PrologObject)solStack.pop(), m_solList);
        }
            
        if(m_solList == null)
            m_solList = List.NIL;
        
        m_wam.closeQuery();
    
        term.clear();
        
        return m_res.unify(m_solList, varsTbl);
    }
    
    public final boolean isDeterministic()
    {
        return m_wam == null; // never started
    }
    
    private final WAM getNewWAM()
    {
        if(getWAM() instanceof WAMTrace)
            return new WAMTrace((WAMTrace)getWAM());
        else
            return new WAM(m_jipEngine);
    }
}
