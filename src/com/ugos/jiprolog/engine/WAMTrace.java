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

class WAMTrace extends WAM
{
    private EventNotifier m_eventNotifier;
    private Node          m_callToSkip;
    private Node          m_lastExitNode;
    private boolean       m_bNotifyRedo;

    WAMTrace(final JIPEngine engine)
    {
        super(engine);
        m_eventNotifier = engine.getEventNotifier();
        m_bNotifyRedo = true;
    }

    WAMTrace(final WAMTrace wam)
    {
        super(wam);
        m_eventNotifier = wam.m_eventNotifier;
        m_bNotifyRedo = true;
    }

    @Override
    final Node backtrack(Node curNode)
    {
        Node backtrack;
        while(curNode != null)
        {
//            if(m_bNotifyRedo)
//                notifyRedo(curNode);

            // risale l'albero saltando i punti di backtracking
            if(curNode.m_backtrack != null)
            {
                backtrack = curNode.m_backtrack;
                do
                {
                    // Azzera le variabili eventualmente istanziate al livello corrente
                    // poiché in hasMoreElements viene riprovato il match
                    curNode.clearVariables();

                    // call precedente
                    curNode = curNode.m_previous;

//                    if(m_bNotifyRedo)
//                        notifyRedo(curNode);

//                    //aggiorna il backtraking
                    if(curNode.m_backtrack != null)
                    {
                        if(curNode.m_backtrack.m_nLevel < backtrack.m_nLevel)
                        {
                            backtrack = curNode.m_backtrack;                                                //
                        }
                    }
                }
                while(backtrack != curNode);
            }

            if(m_bNotifyRedo)
                notifyRedo(curNode);

            curNode.clearVariables();

            if(curNode == m_rootNode)
            {
                // non ho altro backtracking
                return null;
            }

            if(curNode.getGoal() instanceof BuiltInPredicate)
            {
                if(((BuiltInPredicate)curNode.getGoal()).hasMoreChoicePoints())
                    return curNode;
            }
//            else if(curNode.m_ruleEnum == null)
//            {
////            	System.out.println(curNode.getGoal());
//            }
            else if(curNode.m_ruleEnum.hasMoreElements())
            {
                // se la prossima regola unificante fallisce nel corpo qui non
                // è possibile accorgersene (il risultato cioè non è deterministico)
                // quindi hasMoreElement ritorna true
                // occorrerebbe eseguire un look-haed per verificare se la prossima
                // regola unifica

                return curNode;
            }

            curNode = curNode.m_previous;
        }

        return null;
    }

    @Override
    boolean run(Node curNode)
    {
        PrologRule  rule = null;
        Clause      clause = null;
        boolean     bUnify = false;
        Hashtable   varTbl = null;
        Node        newNode = null;
        Node        parentNode;
        int         nCallCount = m_nBaseCounter;

        try
        {
            while(curNode != null)
            {
                m_curNode = curNode;

                try
                {
	                bUnify = false;

	                // genera le clausole che unificano
	                // se le clausole sono state già generate siamo in backtracking
	                // altrimenti ne genera di nuove
	                if(curNode.m_ruleEnum == null)
	                {
	                    try
	                    {
	                        curNode.m_ruleEnum = getRules(curNode);
	                    }
	                    catch(UndefinedPredicateException ex)
	                    {
//	                    	ex.printStackTrace();
//	                    	ex.printPrologStackTrace();

	                        // invia il warning se il predicato non è definito
	                        // e non è dynamic
	                        // in questo caso la enumeration deve essere vuota
	                        if(!m_globalDB.isDynamic(((Functor)ex.getCulprit()).getName()))
	                        {
	                        	String unknown = (String)m_engine.getEnvVariable("unknown");
	                        	if(unknown.equals("warning"))
	                        	{
		                            ex.m_curNode = curNode;
		                            m_engine.notifyEvent(JIPEvent.ID_UNDEFPREDICATE, Atom.createAtom(ex.getPredicateName()), hashCode());
	                        	}
	                        	else if(unknown.equals("error"))
	                        	{
	                        		throw JIPExistenceException.createProcedureException(((Functor)ex.getCulprit()).getPredicateIndicator());
	                        	}
	                        }

	                        curNode.m_ruleEnum = s_emptyEnum;
	                    }
	                }

	                nCallCount++;
	                curNode.m_nLevel = nCallCount;

	                // call
	                notifyCall(curNode);

	                varTbl = new Hashtable(13); // imposta l'hashtable per le variabili

	                while(curNode.m_ruleEnum.hasMoreElements())
	                {
	                    rule   = (PrologRule)curNode.m_ruleEnum.nextElement();
	                    clause = rule.m_cons;
	                    // UNIFY
	                    // unifica la testa della clausola con il predicato corrente
	                    if(bUnify = curNode.getGoal().unify(clause.getHead(), varTbl))
	                    	break;
	                }
                }
                catch(JIPRuntimeException ex)
                {
//                	ex.printStackTrace();

                	while(!exceptionListenerStack.isEmpty())
                    {
                		ExceptionListener exceptionListener = exceptionListenerStack.pop();
                    	if(exceptionListener.notifyException(ex))
                    	{
	                    	curNode = m_curNode;
	                    	bUnify = true;
	                    	break;
                    	}
                    }

                	if(!bUnify)
                	{
                		throw ex;
                	}
                }

                if(bUnify && notifyFound(rule.m_dbCons, curNode))
                {
                    notifyBound(curNode);

                    // imposta l'hashtable delle variabili instanziate nel nodo corrente
                    curNode.m_varTbl = varTbl;

                    // FOUND
                    //System.out.println("curNode call list  " + curNode.m_callList);  // dbg
                    if(clause.getTail() != null) // the clausole has a body
                    {
                        // create a new node
                        newNode = new Node((ConsCell)clause.getTail(), curNode, curNode, rule.m_strModule);
                    }
                    else if(curNode.m_injectedBody != null)  // the clausole has an injected body
                    {
                        // create a new node
                        newNode = new Node(curNode.m_injectedBody, curNode, curNode, rule.m_strModule);
                        curNode.m_injectedBody = null;
                    }
                    else if(curNode.m_callList.getTail() != null) // la clausola non ha un body continuo con il resto
                    {
                    	if(!moduleStack.isEmpty())
                    		moduleStack.pop();

                        // crea un nuovo nodo
                        newNode = new Node((ConsCell)curNode.m_callList.getTail(), curNode.m_parent, curNode, curNode.m_strModule);
                    }
                    else  // torna al parent
                    {
                    	notifyExit(curNode);

                        newNode = null;

                        parentNode = curNode.m_parent;

                        while(newNode == null && parentNode != null)
                        {
                        	if(!moduleStack.isEmpty())
                        		moduleStack.pop();

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

//                        newNode.m_nLevel = m_curNode.m_nLevel + 1;
                    }

                    curNode = newNode;
                }
                else
                {
//                	System.out.println("fail " + curNode.getGoal());
                    // FAIL
                    // non ci sono clausole unificanti
                    // BACKTRACK
                	notifyFail(curNode);

                	if(!moduleStack.isEmpty())
                		moduleStack.pop();

//                    System.out.println("Fail " + curNode.getGoal());  // dbg
                    curNode.m_ruleEnum = null;
                    curNode.clearVariables();
                    curNode = backtrack(curNode.m_previous);
                }
            }
        }
        catch(JIPRuntimeException ex)
        {
//            ex.printStackTrace();  //DBG

        	if(curNode.getGoal() instanceof BuiltInPredicate)
            	((BuiltInPredicate)curNode.getGoal()).deinit();

            m_curNode = null;
            m_startNode = null;
            m_lastNode = null;
            ex.m_curNode = curNode;
            ex.m_engine = m_engine;

            if(rule != null)
            {
                final Clause cla = ((Clause)rule.m_dbCons);
                if(cla != null)
                {
                    ex.m_strFileName = cla.getFileName(); // nel caso di built-in non è valorizzato
                    ex.m_nLineNumber = cla.getLine();
                    ex.m_nPosition = cla.getPosition();
                }
            }
            throw ex;
        }
//        catch(StackOverflowError er)
//        {
//
////            notifyStop();
//            //er.printStackTrace();   //DBG
//
//            m_curNode = null;
//            m_lastNode = null;
//            m_startNode = null;
//            JIPJVMException ex = new JIPJVMException(er);
//
//            ex.m_curNode = curNode;
//            ex.m_engine = m_engine;
//            if(rule != null)
//            {
//                final Clause cla = ((Clause)rule.m_dbCons);
//                if(cla != null)
//                {
//                    ex.m_strFileName = cla.getFileName();  // nel caso di built-in non è valorizzato
//                    ex.m_nLineNumber = cla.getLineNumber();
//                    ex.m_nPosition   = cla.getPosition();
//                }
//            }
//            throw ex;
//        }
//        catch(ClassCastException ex)
//        {
//            ex.printStackTrace();
//
//            if(curNode.getGoal() instanceof BuiltInPredicate)
//            	((BuiltInPredicate)curNode.getGoal()).deinit();
//
//            m_curNode = null;
//            m_lastNode = null;
//            m_startNode = null;
//            JIPRuntimeException ex1 = JIPRuntimeException.createRuntimeException(29);//curNode.getGoal());
//            ex1.m_curNode = curNode;
//            ex1.m_engine = m_engine;
//            if(rule != null)
//            {
//                final Clause cla = ((Clause)rule.m_dbCons);
//                if(cla != null)
//                {
//                    ex1.m_strFileName = cla.getFileName();
//                    ex1.m_nLineNumber = cla.getLine();
//                    ex1.m_nPosition = cla.getPosition();
//                }
//            }
//            throw ex1;
//        }
        catch(Throwable th)
        {
            th.printStackTrace();   //DBG

            if(curNode.getGoal() instanceof BuiltInPredicate)
            	((BuiltInPredicate)curNode.getGoal()).deinit();

            m_curNode = null;
            m_lastNode = null;
            m_startNode = null;
            JIPJVMException ex = new JIPJVMException(th);

            ex.m_curNode = curNode;
            ex.m_engine = m_engine;
            if(rule != null)
            {
                final Clause cla = ((Clause)rule.m_dbCons);
                if(cla != null)
                {
                    ex.m_strFileName = cla.getFileName();
                    ex.m_nLineNumber = cla.getLine();
                    ex.m_nPosition   = cla.getPosition();
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
    @Override
    final boolean query(final PrologObject query)//, final SolutionConsumer consumer)
        throws JIPIsRunningException
    {
        m_bNotifyRedo = true;

        notifyStart();

        return super.query(query);
    }

    @Override
    final boolean nextSolution()
        throws JIPIsRunningException, JIPQueryClosedException
    {
        m_bNotifyRedo = false;
        backtrack(m_lastNode);

        m_bNotifyRedo = true;
        return super.nextSolution();
    }

    @Override
    final boolean hasMoreChoicePoints()
        throws JIPIsRunningException, JIPQueryClosedException
    {
        m_bNotifyRedo = false;
        return super.hasMoreChoicePoints();
    }

    final void notifyCall(final Node call)
    {
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
