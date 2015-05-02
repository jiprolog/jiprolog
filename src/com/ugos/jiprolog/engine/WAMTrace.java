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
import java.util.Hashtable;

class WAMTrace extends WAM
{
    private EventNotifier m_eventNotifier;

    private boolean       m_bSkip = false;
    private Node          m_callToSkip;
    //private int           m_nInvocationNumber;
    private Node          m_lastExitNode;
    private boolean       m_bNotifyRedo;

    WAMTrace(final JIPEngine engine)
    {
        super(engine);
        m_eventNotifier = engine.getEventNotifier();
        //m_nInvocationNumber = 0;
        m_bNotifyRedo = true;
    }

    WAMTrace(final WAMTrace wam)
    {
        super(wam);
        m_eventNotifier = wam.m_eventNotifier;
        //m_nInvocationNumber = wam.m_nInvocationNumber ;
        m_bNotifyRedo = true;
    }

    final Node backtrack(Node curNode)
    {
        while(curNode != null)
        {
            if(m_bNotifyRedo)
                notifyRedo(curNode);

            // risale l'albero saltando i punti di backtracking
            if(curNode.m_backtrack != null)
            {
                Node backtrack = curNode.m_backtrack;;
                do
                {
                    // Azzera le variabili eventualmente istanziate al livello corrente
                    // poiché in hasMoreElements viene riprovato il match
                    curNode.clearVariables();

                    // call precedente
                    curNode = curNode.m_previous;

                    if(m_bNotifyRedo)
                        notifyRedo(curNode);

                    //System.out.println("redo2: " + curNode.getGoal());

                    //aggiorna il backtraking
                    if(curNode.m_backtrack != null)
                    {
                        if(curNode.m_backtrack.m_nLevel < backtrack.m_nLevel)
                            backtrack = curNode.m_backtrack;
                    }
                }
                while(backtrack != curNode);
            }

            //boolean bMore;
            // Azzera le variabili eventualmente istanziate al livello corrente
            // poiché in hasMoreElements viene riprovato il match
            curNode.clearVariables();

            if(curNode == m_rootNode)
            {
                // non ho altro backtracking
//              System.out.println("root node reached");
                return null;
            }

            if(curNode.getGoal() instanceof BuiltInPredicate)
            {
                if(((BuiltInPredicate)curNode.getGoal()).hasMoreChoicePoints())
                    return curNode;
            }
            else if(curNode.m_ruleEnum.hasMoreElements())
            {
                // se la prossima regola unificante fallisce nel corpo qui non
                // è possibile accorgersene (il risultato cioè non è deterministico)
                // quindi hasMoreElement ritorna true
                return curNode;
            }

            curNode = curNode.m_previous;
        }

        return null;
    }

    boolean run(Node curNode)
    {
        PrologRule  rule = null;
        ConsCell    clause = null;
        boolean     bUnify;
        Hashtable   varTbl;
        Node        newNode = null;
        Node        parentNode;
        int         nCallCount = m_nBaseCounter;

        try
        {
            while(curNode != null)
            {
                if(m_startNode == null)
                	throw new JIPAbortException();

                m_curNode = curNode;

//              System.out.println("goal " + curNode.m_callList.getHead());  // dbg
//              System.out.println("currentModule " + curNode.m_strModule);  // dbg

                // genera le clausole che unificano
                // se le clausole sono state già generate siamo in backtracking
                // altrimenti ne genera di nuove
                if(curNode.m_ruleEnum == null)
                {
                    try
                    {
                        //PrologObject obj = curNode.getGoal();
//                      System.out.println("Call  " + obj);  // dbg
//                        System.out.println("Call  " + curNode.getGoal().getClass());  // dbg
                        curNode.m_ruleEnum = getRules(curNode);
//                        System.out.println("Call3  " + obj);  // dbg
//                        System.out.println("Call2  " + curNode.getGoal());  // dbg
                    }
                    catch(UndefinedPredicateException ex)
                    {
                        // invia il warning se il predicato non è definito
                        // in questo caso la enumeration deve essere vuota
                        if(!m_globalDB.isDynamic(ex.getPredicateName()))
                        {
                            ex.m_curNode = curNode;
                            //m_engine.notifyException(ex, hashCode());
                            m_engine.notifyEvent(JIPEvent.ID_UNDEFPREDICATE, Atom.createAtom(ex.getPredicateName()), hashCode());
                        }

                        curNode.m_ruleEnum = s_emptyEnum;
                    }
                }

                nCallCount++;
                curNode.m_nLevel = nCallCount;

                // call
                notifyCall(curNode);

                bUnify = false;
                varTbl = new Hashtable(5,1); // imposta l'hashtable per le variabili
                while(curNode.m_ruleEnum.hasMoreElements() && !bUnify)
                {
                    rule   = (PrologRule)curNode.m_ruleEnum.nextElement();
                    clause = rule.m_cons;
//                  System.out.println("Unify " + curNode.getGoal());
//                  System.out.println("Unify " + clause.getHead() + " " + clause);
//                  System.out.println("CUnify " + curNode.getGoal().getClass());
//                  System.out.println("CUnify " + clause.getHead().getClass());
                    // UNIFY
                    // unifica la testa della clausola con il predicato corrente
                    bUnify = curNode.getGoal().unify(clause.getHead(), varTbl);
//                  System.out.println("bUnify " + bUnify);
                }



                // verifica la presenza di almeno una clausola
                if(bUnify && notifyFound(rule.m_dbCons, curNode))
                {
                    notifyBound(curNode);

                    // imposta l'hashtable delle variabili instanziate nel nodo corrente
                    curNode.m_varTbl = varTbl;

                    // FOUND
                    //System.out.println("curNode call list  " + curNode.m_callList);  // dbg
                    newNode = null;
                    if(clause.getTail() != null)
                    {
//                      System.out.println("clause.getTail() != null");  // dbg
//                      System.out.println("clause.getTail() " + clause.getTail());  // dbg

                        // crea un nuovo nodo
                        newNode = new Node((ConsCell)clause.getTail(), curNode, curNode, rule.m_strModule);
                    }
                    else if(curNode.m_callList.getTail() != null)
                    {
                        // crea un nuovo nodo
                        newNode = new Node((ConsCell)curNode.m_callList.getTail(), curNode.m_parent, curNode, curNode.m_strModule);
                    }
                    else
                    {
                        notifyExit(curNode);
                        parentNode = curNode.m_parent;
                        //System.out.println("parentNode" + parentNode.m_callList);

                        while(newNode == null && parentNode != null)
                        {
                            if(((ConsCell)parentNode.m_callList.getTail()) != null)
                            {
                                newNode = new Node((ConsCell)parentNode.m_callList.getTail(), parentNode.m_parent, curNode, parentNode.m_strModule);
                            }
                            else
                            {
                                notifyExit(parentNode);
                                parentNode = parentNode.m_parent;
                            }
                        }

                        if(newNode == null)
                        {
                            m_lastNode = curNode;
                            m_curNode = null;
                            return true;
                        }
                    }

                    curNode = newNode;
                }
                else
                {
                    // FAIL
                    // non ci sono clausole unificanti
                    // BACKTRACK
                    notifyFail(curNode);


                    //System.out.println("Fail " + curNode.getGoal());  // dbg
                    curNode.m_ruleEnum = null;
                    curNode.clearVariables();
                    curNode = backtrack(curNode.m_previous);
                }
            }
        }
        catch(JIPRuntimeException ex)
        {
//            notifyStop();
            //ex.printStackTrace();  //DBG

            m_curNode = null;
            m_startNode = null;
            m_lastNode = null;
            ex.m_curNode = curNode;
            if(rule != null)
            {
                final Clause cla = ((Clause)rule.m_dbCons);
                if(cla != null)
                {
                    ex.m_strFileName = cla.getFileName();
                    ex.m_nLineNumber = cla.getLine();
                    ex.m_nPosition = cla.getPosition();
                    ex.m_engine = m_engine;
                }
            }
            throw ex;
        }
//        catch(StackOverflowError er)
//        {
////            notifyStop();
////            er.printStackTrace();   //DBG
//
//            m_curNode = null;
//            m_lastNode = null;
//            m_startNode = null;
//            JIPJVMException ex = new JIPJVMException(er);
//            ex.m_curNode = curNode;
//            if(rule != null)
//            {
//                final Clause cla = ((Clause)rule.m_dbCons);
//                if(cla != null)
//                {
//                    ex.m_strFileName = cla.getFileName();
//                    ex.m_nLineNumber = cla.getLineNumber();
//                    ex.m_nPosition   = cla.getPosition();
//                    ex.m_engine      = m_engine;
//                }
//            }
//            throw ex;
//        }
        catch(ClassCastException ex)
        {
//            ex.printStackTrace();

            m_curNode = null;
            m_lastNode = null;
            m_startNode = null;
            JIPRuntimeException ex1 = JIPRuntimeException.createRuntimeException(29);//curNode.getGoal());
            ex1.m_curNode = curNode;
            if(rule != null)
            {
                final Clause cla = ((Clause)rule.m_dbCons);
                if(cla != null)
                {
                    ex1.m_strFileName = cla.getFileName();
                    ex1.m_nLineNumber = cla.getLine();
                    ex1.m_nPosition = cla.getPosition();
                    ex1.m_engine    = m_engine;
                }
            }
            throw ex1;
        }
        catch(Throwable th)
        {
//            th.printStackTrace();   //DBG

            m_curNode = null;
            m_lastNode = null;
            m_startNode = null;
            JIPJVMException ex = new JIPJVMException(th);
            ex.m_curNode = curNode;
            if(rule != null)
            {
                final Clause cla = ((Clause)rule.m_dbCons);
                if(cla != null)
                {
                    ex.m_strFileName = cla.getFileName();
                    ex.m_nLineNumber = cla.getLine();
                    ex.m_nPosition   = cla.getPosition();
                    ex.m_engine      = m_engine;
                }
            }
            throw ex;
        }

        m_lastNode = m_curNode;
        m_curNode = null;

        notifyStop();

        return false;
    }

    // run a query
    final boolean query(final PrologObject query)//, final SolutionConsumer consumer)
        throws JIPIsRunningException
    {
        m_bNotifyRedo = true;

        notifyStart();

        return super.query(query);
    }

    final boolean nextSolution()
        throws JIPIsRunningException, JIPQueryClosedException
    {
        m_bNotifyRedo = false;
        //final Node curNode = backtrack((Node)m_lastNode);
        backtrack(m_lastNode);

        m_bNotifyRedo = true;
        //notifyRedo(curNode);

        return super.nextSolution();
    }

    final boolean hasMoreChoicePoints()
        throws JIPIsRunningException, JIPQueryClosedException
    {
        m_bNotifyRedo = false;
        return super.hasMoreChoicePoints();
    }

    final void notifyCall(final Node call)
    {
        //m_nInvocationNumber++;
        //call.m_nLevel = m_nInvocationNumber;

        m_lastExitNode = null;

        if(m_callToSkip != null)
            return;

        if(!traceable(call.getGoal()))
            return;

        final JIPTraceEvent ev =
            notifyTraceEvent(JIPTraceEvent.ID_CALL, call.getGoal(), call.m_nLevel);

        waitForUserInput();

        if(ev.executionAborted())
        	throw new JIPAbortException();

        if(ev.skipped())
            m_callToSkip = call;
    }

    final boolean notifyFound(final ConsCell clause, final Node call)
    {
        m_lastExitNode = null;

        if(m_callToSkip != null)
            return true;

        if(!traceable(call.getGoal()))
            return true;

        JIPTraceEvent ev =
            notifyTraceEvent(JIPTraceEvent.ID_FOUND, clause, call.m_nLevel);

        waitForUserInput();

        if(ev.executionAborted())
        	throw new JIPAbortException();
//            throw JIPRuntimeException.create(0, null);

        return !ev.retryCall();
    }

    final void notifyBound(final Node call)
    {
        m_lastExitNode = null;

        if(m_callToSkip != null)
            return;

        if(!traceable(call.getGoal()))
            return;

        JIPTraceEvent ev = notifyTraceEvent(JIPTraceEvent.ID_BIND, call.getGoal(), call.m_nLevel);

        waitForUserInput();

        if(ev.executionAborted())
        	throw new JIPAbortException();
    }

    final void notifyExit(final Node call)
    {
        if(m_callToSkip == call)
        {
//            Debug.traceln("skipped found " + call,1);
            m_callToSkip = null;
        }
        else if(m_callToSkip != null)
        {
//            Debug.traceln("skipped not found " + m_callToSkip,1);
            return;
        }

        if (m_lastExitNode == call)
            return;

        m_lastExitNode = call;

        if(!traceable(call.getGoal()))
            return;

        //JIPTraceEvent ev = notifyTraceEvent(JIPTraceEvent.ID_EXIT, call.m_pred, call.m_parentNode.m_nLevel +  + m_nBaseLevel);
        JIPTraceEvent ev = notifyTraceEvent(JIPTraceEvent.ID_EXIT, call.getGoal(), call.m_nLevel);

        waitForUserInput();

        if(ev.executionAborted())
        	throw new JIPAbortException();
    }

    final void notifyFail(final Node call)
    {
        m_lastExitNode = null;

        if(m_callToSkip == call)
        {
            m_callToSkip = null;
        }
        else if(m_callToSkip != null)
        {
            return;
        }

        if(!traceable(call.getGoal()))
            return;

        // Fail Event
        JIPTraceEvent ev =
            notifyTraceEvent(JIPTraceEvent.ID_FAIL, call.getGoal(), call.m_nLevel);

        waitForUserInput();

        if(ev.executionAborted())
        	throw new JIPAbortException();
    }

    final void notifyRedo(final Node call)
    {
        if(call == m_rootNode)
            return;

        m_lastExitNode = null;

        if(m_callToSkip == call)
        {
            m_callToSkip = null;
        }
        else if(m_callToSkip != null)
        {
            return;
        }

        if(!traceable(call.getGoal()))
            return;

        // Redo Event
        JIPTraceEvent ev =
            notifyTraceEvent(JIPTraceEvent.ID_REDO, call.getGoal(), call.m_nLevel);

        waitForUserInput();

        if(ev.executionAborted())
        	throw new JIPAbortException();
    }

    private final JIPTraceEvent notifyTraceEvent(final int nID, final PrologObject term, final int nInvocationNumber)
    {
        //m_nLevel = nLevel;
        return this.m_eventNotifier.notifyTraceEvent(nID, term, hashCode(), this, nInvocationNumber);
    }

    final void notifyStart()
    {
        if(m_nBaseCounter == 0)
            notifyTraceEvent(JIPTraceEvent.ID_START, null, 0);
    }

    final void notifyStop()
    {
        notifyTraceEvent(JIPTraceEvent.ID_STOP, null, 0);
    }

    private synchronized final void waitForUserInput()
    {
        try
        {
            wait();
        }
        catch(InterruptedException ex)
        {
            throw new JIPJVMException(ex);
        }
    }

    synchronized final void notifyUserInput()
    {
        notify();
    }

    final boolean traceable(PrologObject obj)
    {
        if(getJIPEngine().getEnvVariable("__trace__") == null)
        {
            final Hashtable spyTable = (Hashtable)getJIPEngine().getEnvVariable("__spy__");
            if(spyTable == null)
            {
                return false;
            }

            obj = BuiltIn.getRealTerm(obj);

            if(obj instanceof Functor)
                return spyTable.containsKey(((Functor)obj).getName()) || spyTable.containsKey(((Functor)obj).getFriendlyName());
            else if(obj instanceof ConsCell)
                return traceable(((ConsCell)obj).getHead());
            else
                return spyTable.containsKey(((Atom)obj).getName()) || spyTable.containsKey(((Atom)obj).getName() + "/0");
        }
        else if(obj instanceof Functor)
        {
        	if(getJIPEngine().isInternal(((Functor) obj).getName()))
        		return false;
        }

        return true;
    }
}
