package com.ugos.JIProlog.engine;

import java.util.*;

final class IfThenElse3 extends BuiltIn
{
    private WAM          m_wam = null;
    
    public final boolean unify(final Hashtable varsTbl)
    {
        if(m_wam != null)
        {
            if(m_wam.nextSolution())
            {
                if(!m_wam.hasMoreChoicePoints())
                {
                    m_wam.closeQuery();
                    m_wam = null;    
                }
                
                return true;
            }
            else
                return false;  
        }
        
        PrologObject ift   = getRealTerm(getParam(1));
        PrologObject thent = getRealTerm(getParam(2));
        PrologObject elset = getRealTerm(getParam(3));
                    
        if(ift == null)
            throw new JIPParameterUnboundedException(1);
        
        m_wam = getNewWAM();
        
        if(m_wam.query(ift))
        {
            //execute then
            m_wam.closeQuery();
            if(thent== null)
                throw new JIPParameterUnboundedException(2);
            
            if(m_wam.query(thent))
            {
                if(!m_wam.hasMoreChoicePoints())
                {
                    m_wam.closeQuery();
                    m_wam = null;    
                }
                
                return true;
            }
            else
                return false; 
        }
        else
        {
            // execute else
            m_wam.closeQuery();
           
            if(elset== null)
                throw new JIPParameterUnboundedException(3);
            
            if(m_wam.query(elset))
            {
                if(!m_wam.hasMoreChoicePoints())
                {
                    m_wam.closeQuery();
                    m_wam = null;    
                }
                
                return true;
            }
            else
                return false; 
        }
    }
    
    public final boolean hasMoreChoicePoints()
    {
        return m_wam != null;  // mai partita
    }
    
    private final WAM getNewWAM()
    {
        if(getWAM() instanceof WAMTrace)
            return new WAMTrace((WAMTrace)getWAM());
        else
            return new WAM(getWAM());
            //return new WAM(m_jipEngine);
    }
}
