/*
 * 09/19/2002
 *
 * Copyright (C) 2002 Ugo Chirico
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

package com.ugos.JIProlog.extensions.exception;

import com.ugos.JIProlog.engine.*;
import java.io.*;
import java.util.*;

public class JIPCatch3 extends JIPXCall
{
    private JIPQuery   m_jipQuery     = null;
    private JIPTerm    m_thrownTerm   = null;
    private JIPTerm    m_mask         = null;
    private JIPEngine  m_engine       = null;
    private JIPQuery   m_recoverQuery = null;
    
    public final synchronized boolean unify(final JIPCons params, Hashtable varsTbl)
    {
        JIPTerm solution;
        // Check if recover is running
        if(m_recoverQuery != null)
        {
            solution = m_recoverQuery.nextSolution();
            if(solution != null)
            {
            	if(params.getNth(1).unify(solution, varsTbl))
                    return true;
                
                m_recoverQuery.close();
                m_recoverQuery = null;
                return false;
            }
        }
                
        // Open Query
        // Check if goal is running
        if(m_jipQuery == null)
        {
            //  Extract inputs
            JIPTerm goal = null;
            
            goal = params.getNth(1);
            if (goal instanceof JIPVariable)
            {
                // try to extract the term
                if(!((JIPVariable)goal).isBounded())
                {
                    throw new JIPParameterUnboundedException(1);
                }
                else
                {
                    //extracts the term
                    goal = ((JIPVariable)goal).getValue();
                }
            }
    
            m_mask = params.getNth(2);
                                    
            m_engine = getJIPEngine();
            
            m_jipQuery = m_engine.openSynchronousQuery(goal);
        }
        
        // Run Query
        try
        {
            solution = m_jipQuery.nextSolution();
            if(solution == null)
            {
                m_jipQuery.close();
                m_jipQuery = null;
                return false;
            }
        }
        catch(JIPRuntimeException ex)
        {
            //ex.printPrologStackTrace();
            
            // Error found
            m_thrownTerm = ex.getTerm();
            if(m_thrownTerm == null)
                return false;
            
            //System.out.println("m_thrownTerm " + m_thrownTerm);
            //System.out.println("m_mask " + m_mask);
            
            // close the query
            m_jipQuery.close();
            m_jipQuery = null;
            
            // Check if unify with mask
            if(m_thrownTerm.unify(m_mask, varsTbl))
            {
                //System.out.println("UNIFY");
                JIPTerm recoverGoal = params.getNth(3);
                if (recoverGoal instanceof JIPVariable)
                {
                    // try to extract the term
                    if(!((JIPVariable)recoverGoal).isBounded())
                    {
                        throw new JIPParameterUnboundedException(3);
                        //throw new JIPRuntimeException(JIPErrors.ERR_UNBOUNDED, JIPErrors.STR_UNBOUNDED, getPredicate());
                    }
                    else
                    {
                        //extracts the term
                        recoverGoal = ((JIPVariable)recoverGoal).getValue();
                    }
                }

                // Run recover goal
                m_recoverQuery = m_engine.openSynchronousQuery(recoverGoal);
                solution = m_recoverQuery.nextSolution();
                
                if(solution != null)
                {
                    if(params.getNth(3).unify(solution, varsTbl))
                        return true;
               
                    m_recoverQuery.close();	
                    m_recoverQuery = null;
                    return false;
                }
            }
            else
            {
                //System.out.println("NOT UNIFY");
                throw ex;
            }
        }
        
        //System.out.println(params.getNth(1));
        //System.out.println("solution " + solution);
        
        if(params.getNth(1).unify(solution, varsTbl))
            return true;
        
        // close the query
        m_jipQuery.close();
        m_jipQuery = null;
        
        return false;
    }
            
    public boolean hasMoreChoicePoints()
    {
        if(m_recoverQuery != null)
        {
            return m_recoverQuery.hasMoreChoicePoints();
        }
                    
        if(m_jipQuery != null)
        {
            return m_jipQuery.hasMoreChoicePoints();
        }
        
        return false;
    }
}

