package com.ugos.jiprolog.engine;

import java.util.Hashtable;
import java.util.Stack;
import java.util.Vector;
//import com.ugos.debug.*;

class BagOf3 extends BuiltIn
{
    Vector  m_solVect;
    WAM m_wam;
    
    public final boolean unify(final Hashtable varsTbl)
    {
        PrologObject term  = getParam(1);        
        if(term instanceof Variable)
        	term = ((Variable)term).getObject();
        
        PrologObject query = getParam(2);
        if(query == null)
            throw new JIPParameterUnboundedException(2);
        
        PrologObject res = getParam(3);
        
        
        // separa le variabili non libere dalla query
        final ConsCell vars = extractVars(query);
        final Stack solStack = new Stack();
        
        // cancella le variabili non libere
        term.clear();
        if(vars != null)
            vars.clear();
        
        // colleziona le soluzioni        
        m_solVect = getSolutions(query);
        
        int i = 0;
        while(i < m_solVect.size())
        {
            if(query.unify((PrologObject)m_solVect.elementAt(i), varsTbl))
            {                
                solStack.push(term.copy());
                m_solVect.removeElementAt(i);
                // cancella le variabili non libere
                term.clear();
                if(vars != null)
                    vars.clear();
            }
            else
            {
                i++;
            }
        }
        
        List solList = null;
            
        while(!solStack.isEmpty())
        {
            solList = new List((PrologObject)solStack.pop(), solList);
        }
            
        if(solList == null)
            solList = List.NIL;
        
        return res.unify(solList, varsTbl);
    }    
        
    public final boolean isDeterministic()
    {
        return false;
    }
        
    final Vector getSolutions(PrologObject query)
    {        
        m_wam = getNewWAM();
        
        Vector solVect = new Vector();
        if(m_wam.query(new ConsCell(query, null)))
        {
            solVect.addElement(query.copy());
            while(m_wam.nextSolution())
            {
                solVect.addElement(query.copy());
            }
        }
                    
        m_wam.closeQuery();
        
        return solVect;
    }

    private final WAM getNewWAM()
    {
        if(getWAM() instanceof WAMTrace)
            return new WAMTrace((WAMTrace)getWAM());
        else
            return new WAM(m_jipEngine);
    }
        
    final ConsCell extractVars(PrologObject obj)
    {
        if(obj instanceof Functor)
        {
            if (((Functor)obj).getFriendlyName().equals("^"))
                return new ConsCell(((Functor)obj).getParams().getTerm(1), null);
            else
                return null;
        }
        else if(obj instanceof ConsCell)
        {
            ConsCell head = extractVars(((ConsCell)obj).getHead());
            if(head != null)
                return ConsCell.append(head, (ConsCell)extractVars(((ConsCell)obj).getTail()));
            else
                return extractVars(((ConsCell)obj).getTail());
        }
        else
            return null;
    }
}
         
        
