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
//import com.ugos.debug.*;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Stack;
import java.util.Vector;

class WAM
{
    // global db
    GlobalDB m_globalDB;

    String m_strBaseModule;

    Node      m_startNode;
    Node      m_curNode;
    Node      m_lastNode;
    Node      m_rootNode = new Node(ConsCell.NIL, null, null, "none");

    JIPEngine m_engine;

    int       m_nBaseCounter;
    boolean   m_bClosed = false;

    static final Enumeration s_emptyEnum = new Vector(1).elements();

    protected class Node
    {
        protected ConsCell     m_callList;
        protected Node         m_parent;
        protected Node         m_previous;
        protected String       m_strModule;
        protected int          m_nLevel;
        protected Node         m_backtrack;
        protected Enumeration  m_ruleEnum;

        protected Hashtable    m_varTbl;

        Node(final ConsCell callList, final Node parent, final Node previous, final String strModule)
        {
            m_callList  = callList;
            m_parent    = parent;
            m_strModule = strModule;
            m_previous  = previous;
        }

        final PrologObject getGoal()
        {
            return m_callList.getHead();
        }

        final void setGoal(final PrologObject goal)
        {
            m_callList.setHead(goal);
        }

        final void clearVariables()
        {
            if(m_varTbl != null)
            {
//                System.out.println(m_varTbl);
                final Enumeration en = m_varTbl.elements();
                while (en.hasMoreElements())
                        ((Clearable)en.nextElement()).clear();
                m_varTbl = null;
            }
        }
//        public String toString() {return m_callList.toString();}
    }

    WAM(final JIPEngine jipEngine)
    {
        m_globalDB         = jipEngine.getGlobalDB();
        m_engine           = jipEngine;
        m_strBaseModule    = GlobalDB.USER_MODULE;
        m_nBaseCounter     = 0;
    }

    WAM(final WAM wam)
    {
        m_globalDB         = wam.m_globalDB;
        m_engine           = wam.m_engine;
        m_nBaseCounter     = wam.m_nBaseCounter;

        if(wam.m_curNode != null)
            m_strBaseModule = wam.m_curNode.m_strModule;
        else
            m_strBaseModule = GlobalDB.USER_MODULE;
    }

    final Node getCurNode()
    {
        return m_curNode;
    }

    final JIPEngine getJIPEngine()
    {
        return m_engine;
    }

    // run a query
    boolean query(PrologObject query) throws JIPIsRunningException
    {
        if(isRunning() || isWaiting())  // da testare l'inseriento di isWaiting
            throw new JIPIsRunningException();

        query = new ConsCell(query, null);

        m_startNode = new Node((ConsCell)query, null, m_rootNode, m_strBaseModule);

        m_bClosed = false;

        return run(m_startNode);
    }

    boolean hasMoreChoicePoints()
        throws JIPIsRunningException
    {
        // se è running torna exception
        if(isRunning())
            throw new JIPIsRunningException();

        // se è stato chiusa con closeQuery o non è mai partita lancia eccezione
        if(isClosed() || isNeverRun())
            throw new JIPQueryClosedException();;

        // backtrack
        m_lastNode = backtrack(m_lastNode);
        return m_lastNode != null;// && m_lastNode != m_rootNode;

//        if(m_lastNode != null)
//        {
//            System.out.println("hasMoreChoicePoints " + m_lastNode.getGoal());
//
//            return m_lastNode.m_ruleen.isNextUnifiable(m_lastNode.getGoal());
//        }
//
//        return false;
    }

    boolean nextSolution()
        throws JIPIsRunningException, JIPQueryClosedException
    {
        if(isClosed() || isNeverRun())
            throw new JIPQueryClosedException();

        if(isRunning())
            throw new JIPIsRunningException();

        final Node curNode = backtrack(m_lastNode);

        return run(curNode);
    }

    final void closeQuery()
    {
        // aggiungere thread di rilascio delle risorse
        // nel thread verrebbe chiamata backtrack fino al termine

        m_curNode = null;
        m_lastNode = null;
        m_startNode = null;

        m_bClosed = true;
    }

    // la WAM sta eseguendo il metodo run
    final boolean isRunning()
    {
        return m_curNode != null;
    }

    // la WAM è stata chiusa con closeQuery o dopo errore
    final boolean isClosed()
    {
        return m_bClosed;
    }

    //  la query non è ancora partita
    final boolean isNeverRun()
    {
        return m_startNode == null && !m_bClosed;
    }

    // la WAM è in attesa di ricevere nextSolution o closeQuery
    final boolean isWaiting()
    {
        return !isNeverRun() && !isRunning() && !isClosed();
    }

    final void cut()
    {
        if(m_curNode.m_parent != null)
        {
//            System.out.println("CUT at: " + m_curNode.getGoal());
//            System.out.println("Parent: " + m_curNode.m_parent.getGoal());
//            System.out.println("RootNode: " + m_rootNode);
            Atom atom = ((Functor)(Clause.getClause(m_curNode.m_parent.getGoal()).getHead())).getAtom();
//            System.out.println("Atom : " + atom);
            // se il parent è ;/2 prendo il parent del parent
            //if(strName.equals(";/2") || strName.equals("->/2") || strName.equals("*->/2"))
            if(atom == Atom.FSEMICOLON || atom == Atom.FIF || atom == Atom.FSTARIF)
            //  see §7.8.6.1			see §7.8.7.1
            {
//              System.out.println("Cut: ;/2 - " + m_curNode.getGoal());
              //System.out.println("Parent: " + m_curNode.m_parent.getGoal());
                //m_curNode.m_parent = m_curNode.m_parent.m_parent;
/* *****************************************************
 	            Questa parte era stata aggiunta per cercare di trattare il transparent cut su if-then-else
 	            ma è stato un misunderstanding

 			    Functor funct = ((Functor)(Clause.getClause(m_curNode.m_parent.getGoal()).getHead()));
                if(funct.getParams().getHead() == Atom.FIF || funct.getParams().getHead() == Atom.FSTARIF)
                {
                    if(m_curNode.m_parent.m_previous != null)
                    {
        //              System.out.println("Cut: " + m_curNode.getGoal());
        //              System.out.println("Parent: " + m_curNode.m_parent.getGoal());
                      System.out.println("Previous: " + m_curNode.m_parent.m_previous.getGoal());
        //              System.out.println("Previous : ");
                        m_curNode.m_backtrack = m_curNode.m_parent.m_previous;
                    }
                    else
                    {
        //              System.out.println("No previous: " + m_curNode.getGoal());
                        m_curNode.m_backtrack = m_rootNode;
                    }
                }
                else
                {*/
                	// Il cutparent degli argomenti di ;/2, ->/2 e *-> corrisponde al cutparent dello predicato stesso
                    //System.out.println("Previous: " + m_curNode.m_parent.m_previous.getGoal());
                    if(m_curNode.m_parent.m_parent != null)
                    {
    //                  System.out.println("Parent parent: " + m_curNode.m_parent.m_parent.getGoal());
                        if(m_curNode.m_parent.m_parent.m_previous != null)
                            m_curNode.m_backtrack = m_curNode.m_parent.m_parent.m_previous;
                        else
                            m_curNode.m_backtrack = m_rootNode;
                    }
                    else
                        m_curNode.m_backtrack = m_rootNode;
  //              }
            }
            else if(m_curNode.m_parent.m_previous != null)  // cutparent
            {
//              System.out.println("Cut: " + m_curNode.getGoal());
//              System.out.println("Parent: " + m_curNode.m_parent.getGoal());
//              System.out.println("Previous: " + m_curNode.m_parent.m_previous.getGoal());
//              System.out.println("Previous : ");
                m_curNode.m_backtrack = m_curNode.m_parent.m_previous;
            }
            else
            {
//              System.out.println("No previous: " + m_curNode.getGoal());
                m_curNode.m_backtrack = m_rootNode;
            }

//            System.out.println("CUT till: " + m_curNode.m_backtrack.getGoal());
        }
        else
        {
//           System.out.println("No previous 2: " + m_curNode.getGoal());
            // qui si entra solo se il m_curnode è startNode ed è proprio il cut (?)
            m_curNode.m_backtrack = m_rootNode;
//              System.out.println("CUT till: Start node");
        }

//        if(m_curNode.m_backtrack == null)
//            m_curNode.m_backtrack = m_rootNode;

//        System.out.println("CUT till: " + m_curNode.m_backtrack.getGoal());
    }

    final void scut()
    {
        if(m_curNode.m_parent != null)
        {
            if(m_curNode.m_parent.m_previous != null)  // cutparent
            {
//              System.out.println("Cut: " + m_curNode.getGoal());
//              System.out.println("Parent: " + m_curNode.m_parent.getGoal());
//              System.out.println("Previous: " + m_curNode.m_parent.m_previous.getGoal());
//              System.out.println("Previous : ");
                m_curNode.m_backtrack = m_curNode.m_parent.m_previous;
            }
            else
            {
//              System.out.println("No previous: " + m_curNode.getGoal());
                m_curNode.m_backtrack = m_rootNode;
            }

//            System.out.println("SCUT till: " + m_curNode.m_backtrack.getGoal());
        }
        else
        {
//           System.out.println("No previous 2: " + m_curNode.getGoal());
            // qui si entra solo se il m_curnode è startNode ed è proprio il cut (?)
            m_curNode.m_backtrack = m_rootNode;
//              System.out.println("SCUT till: Start node");
        }

//        System.out.println("SCUT till: " + m_curNode.m_backtrack.getGoal());
    }

    Node backtrack(Node curNode)
    {
        Node backtrack;
        while(curNode != null)
        {
//            System.out.println("redo: " + curNode.getGoal());

            // risale l'albero saltando i punti di backtracking
            if(curNode.m_backtrack != null)
            {
                backtrack = curNode.m_backtrack;
//              System.out.println("cerca1 : " + ((Node)backtrack));
//              System.out.println("cerca2 : " + ((Node)backtrack).getGoal());
                do
                {
                    // Azzera le variabili eventualmente istanziate al livello corrente
                    // poiché in hasMoreElements viene riprovato il match
                    curNode.clearVariables();

                    // call precedente
                    curNode = curNode.m_previous;

//                  System.out.println("curNode.getGoal(): " + curNode.getGoal());
//                  System.out.println("curNode.m_nLevel(): " + curNode.m_nLevel);
////                  if(curNode.m_backtrack != null)
//                      System.out.println("curNode.m_backtrack.getGoal() " + curNode.m_backtrack.getGoal());
//                  else
//                      System.out.println("curNode.m_backtrack.getGoal() null");

//                      System.out.println("backtrack.getGoal() " + backtrack.getGoal());
//                    System.out.println("backtrack.m_nLevel " + backtrack.m_nLevel);

//                    //aggiorna il backtraking
                    if(curNode.m_backtrack != null)
                    {
                        if(curNode.m_backtrack.m_nLevel < backtrack.m_nLevel)
                        {
//                          System.out.println("A: backtrack.getGoal() " + backtrack.getGoal());
//                          System.out.println("A: backtrack.m_nLevel " + backtrack.m_nLevel);
                            backtrack = curNode.m_backtrack;                                                //
                        }
                    }
                }
                while(backtrack != curNode);
            }

          //System.out.println("backtrack : " + curNode.getGoal());
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
//              System.out.println(curNode.getGoal());
                if(((BuiltInPredicate)curNode.getGoal()).hasMoreChoicePoints())
                    return curNode;
            }
            else if(curNode.m_ruleEnum.hasMoreElements())
            {
              //System.out.println("no built in");
              //System.out.println("curNode.m_ruleEnum " + curNode.m_ruleEnum);
//              System.out.println("curNode.m_ruleEnum " + curNode.m_ruleEnum.getClass());
//

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

    boolean run(Node curNode)
    {
        PrologRule  rule = null;
        ConsCell    clause = null;
        boolean     bUnify;
        Hashtable   varTbl;
        Node        newNode = null;
        Node        parentNode;
        int         nCallCount = m_nBaseCounter;
        //PrologRule 	parentRule;
        try
        {
            while(curNode != null)
            {
                // servea a terminare l'esecuzione in seguito alla close
                // trovare un metodo migliore
                if(m_startNode == null)
                {
//                    System.out.println("m_startNode == null");
                    throw JIPRuntimeException.create(0, null);
                }

                m_curNode = curNode;

//              System.out.println("goal " + curNode.getGoal());  // dbg
//              System.out.println("goal " + curNode.m_callList);  // dbg
//              if(curNode.m_parent != null)
//                  System.out.println("parent" + curNode.m_parent.getGoal());  // dbg
//              if(curNode.m_previous != null)
//                  System.out.println("prev " + curNode.m_previous.getGoal());  // dbg

//              System.out.println("currentModule " + curNode.m_strModule);  // dbg

                // genera le clausole che unificano
                // se le clausole sono state già generate siamo in backtracking
                // altrimenti ne genera di nuove
                if(curNode.m_ruleEnum == null)
                {
                    try
                    {
                        curNode.m_ruleEnum = getRules(curNode);
//                        System.out.println("rule en " + curNode.m_ruleEnum);  // dbg
                    }
                    catch(UndefinedPredicateException ex)
                    {
                        // invia il warning se il predicato non è definito
                        // e non è dynamic
                        // in questo caso la eneration deve essere vuota
                        if(!m_globalDB.isDynamic(ex.getPredicateName()))
                        {
                            ex.m_curNode = curNode;
                            m_engine.notifyEvent(JIPEvent.ID_UNDEFPREDICATE, Atom.createAtom(ex.getPredicateName()), hashCode());
                        }

                        curNode.m_ruleEnum = s_emptyEnum;
//                        System.out.println("empty en " + curNode.m_ruleEnum);  // dbg
                    }
                }

                nCallCount++;
                curNode.m_nLevel = nCallCount;

                bUnify = false;
                varTbl = new Hashtable(10); // imposta l'hashtable per le variabili
                while(curNode.m_ruleEnum.hasMoreElements() && !bUnify)
                {
                    rule   = (PrologRule)curNode.m_ruleEnum.nextElement();
                    clause = rule.m_cons;

//                    System.out.println("clause " + clause);  // dbg
                    // UNIFY
                    // unifica la testa della clausola con il predicato corrente
                    bUnify = curNode.getGoal().unify(clause.getHead(), varTbl);
                }

                // verifica la presenza di almeno una clausola
                if(bUnify)
                {
//                    System.out.println("unified " + clause);  // dbg

                    // imposta l'hashtable delle variabili instanziate nel nodo corrente
                    curNode.m_varTbl = varTbl;

                    // FOUND
                    //System.out.println("curNode call list  " + curNode.m_callList);  // dbg
                    newNode = null;
                    if(clause.getTail() != null) // la clausola ha un body
                    {
//                      System.out.println("clause.getTail() != null");  // dbg
//                      System.out.println("clause.getTail() " + clause.getTail());  // dbg

                        // crea un nuovo nodo
                        newNode = new Node((ConsCell)clause.getTail(), curNode, curNode, rule.m_strModule);
                        //parentRule = rule;
                    }
                    else if(curNode.m_callList.getTail() != null) // la clausola non ha un body continuo con il resto
                    {
                        // crea un nuovo nodo
                        newNode = new Node((ConsCell)curNode.m_callList.getTail(), curNode.m_parent, curNode, curNode.m_strModule);
                    }
                    else
                    {
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
//                        System.out.println("m_curNode.getGoal()" + m_curNode.getGoal());
//                        System.out.println("newNode.m_nLevel " +newNode.m_nLevel);
//                        System.out.println("newNode.getGoal() "  + newNode.getGoal());
//                        System.out.println("newNode.m_nLevel " +newNode.m_nLevel);
                    }

                    curNode = newNode;
                }
                else
                {
                    // FAIL
                    // non ci sono clausole unificanti
                    // BACKTRACK
//                    notifyFail(curCall);

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
            ex.printStackTrace();  //DBG

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
                    ex.m_nLineNumber = cla.getLineNumber();
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
        catch(ClassCastException ex)
        {
            //ex.printStackTrace();

            m_curNode = null;
            m_lastNode = null;
            m_startNode = null;
            JIPRuntimeException ex1 = JIPRuntimeException.create(29, null);//curNode.getGoal());
            ex1.m_curNode = curNode;
            ex1.m_engine = m_engine;
            if(rule != null)
            {
                final Clause cla = ((Clause)rule.m_dbCons);
                if(cla != null)
                {
                    ex1.m_strFileName = cla.getFileName();
                    ex1.m_nLineNumber = cla.getLineNumber();
                    ex1.m_nPosition = cla.getPosition();
                }
            }
            throw ex1;
        }
        catch(Throwable th)
        {
            th.printStackTrace();   //DBG

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
                    ex.m_nLineNumber = cla.getLineNumber();
                    ex.m_nPosition   = cla.getPosition();
                }
            }
            throw ex;
        }

        m_lastNode = m_curNode;
        m_curNode = null;

//        notifyStop();

        return false;
    }

    final Enumeration getRules(final Node curNode)
    {
        PrologObject term = curNode.getGoal();

//      System.out.println("getRules " + term);

        // check if variable (used in metacall variable
        if (term instanceof Variable)
        {
            if(((Variable)term).isBounded())
            {
                term = ((Variable)term).getObject();
                curNode.setGoal(term);
            }
            else
            {
                throw JIPRuntimeException.create(23, term);
            }
        }

//      System.out.println(term.getClass());

        // check type
        if (term instanceof Atom)
        {
            term = Functor.getFunctor(term);
            curNode.setGoal(term);
        }

        if (term instanceof BuiltInPredicate)
        {
            return new RulesEnumerationBuiltIn((BuiltInPredicate)term, curNode.m_strModule, this);
        }
        else if (term instanceof Functor)
        {
            String strModule = curNode.m_strModule;
            // controlla se si tratta di :
            if(((Functor)term).getName().equals(":/2"))
            {
                strModule = ((Atom)((Functor)term).getParams().getHead()).getName();
                term = ((ConsCell)((Functor)term).getParams().getTail()).getHead();
                term = Functor.getFunctor(term);
                //PrologObject head = curNode.m_callList.getHead();
//                System.out.println("1 " + head);
                curNode.m_callList.setHead(term);
//                System.out.println("2 " + head);
//                System.out.println("3 " + curNode.m_callList.getHead());
            }

            if (term instanceof BuiltInPredicate)
                return new RulesEnumerationBuiltIn((BuiltInPredicate)term, strModule, this);
            else
                return new RulesEnumeration((Functor)term, strModule, m_globalDB);
        }
        else if (term instanceof List)
        {
            // consult/1
            term = new BuiltInPredicate("consult/1", new ConsCell(term, null));
            curNode.setGoal(term);
            return new RulesEnumerationBuiltIn((BuiltInPredicate)term, curNode.m_strModule, this);
        }
        else if(term instanceof ConsCell)
        {
            PrologObject head = ((ConsCell)term).getHead();
            PrologObject tail = ((ConsCell)term).getTail();

            ConsCell callList = new ConsCell(head, null);
//          System.out.println("HEAD " + head);
//          System.out.println("TAIL " + tail);

            while(tail != null)
            {
              head = ((ConsCell)tail).getHead();
////                System.out.println("HEAD " + head);
              if(head != null)
                  callList = ConsCell.append(callList, new ConsCell(head, null));
//
              tail = ((ConsCell)tail).getTail();
////                System.out.println("TAIL " + tail);
            }

//          System.out.println("curNode.m_callList 1 " + curNode.m_callList);
            curNode.m_callList = ConsCell.append(callList, (ConsCell)curNode.m_callList.getTail());
//          System.out.println("curNode.m_callList 2 " + curNode.m_callList);

//          System.out.println("curNode.m_callList 1 " + curNode.m_callList);
//          System.out.println("curNode.getGoal 1 " + curNode.m_callList);
//          System.out.println("curNode.m_callList.getTail() 1 " + curNode.m_callList.getTail());
//            curNode.m_callList = ConsCell.append((ConsCell)term, (ConsCell)curNode.m_callList.getTail());
//          System.out.println("curNode.m_callList 2 " + curNode.m_callList);

            return getRules(curNode);
        }

      //System.out.println(term);
      //System.out.println(term.getClass());

        throw new JIPParameterTypeException();
    }
}


