/**
 *
 */
package com.ugos.jiprolog.engine;

import java.util.Hashtable;

/**
 * @author UgoChirico
 *
 */
public class Call1 extends BuiltIn {

	WAM wam;

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl)
	{
		PrologObject param = getParam(1);
		if(param instanceof Variable)
		{
			param = ((Variable)param).getObject();
		}

        if(param == null)
		{
        	throw new JIPInstantiationException();
		}
        else if (param instanceof Atom)
        {
        	param = new Functor((Atom)param);
        	if(BuiltInFactory.isBuiltIn(((Functor)param).getName()))
        		param = new BuiltInPredicate(((Functor)param));
        }
        else if (param instanceof Functor)
        {
            if(BuiltInFactory.isBuiltIn(((Functor)param).getName()))
            	param = new BuiltInPredicate(((Functor)param));
        }
		else if(!(param instanceof ConsCell))
		{
			throw new JIPTypeException(JIPTypeException.CALLABLE, param);
		}

//		if(wam == null)
//		{
//			wam = getNewWAM();
//			if(wam.query(param))
//				return true;
//			else
//			{
//				wam.closeQuery();
//				wam = null;
//			}
//		}
//		else
//		{
//			if(wam.nextSolution())
//				return true;
//			else
//			{
//				wam.closeQuery();
//				wam = null;
//			}
//
//		}
//
//		return false;

//		Functor openSnip = new Functor("<!/0", null);
//
//		BuiltInPredicate closeSnip = new BuiltInPredicate("!>/0", null);
//
        // extract the current node
        final WAM.Node curNode = getWAM().getCurNode();
//
        // add the snip and the goal in the call list
        curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(param, curNode.m_callList.m_tail));
//       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(openSnip, new ConsCell(param, new ConsCell(closeSnip, curNode.m_callList.m_tail))));
//       	curNode.m_backtrack = curNode.m_previous.m_previous;

//		BuiltInPredicate softCut = new BuiltInPredicate("$!/0", null);
//
//        // estrae il nodo corrente
//        final WAM.Node curNode = getWAM().getCurNode();
//
//       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(softCut, new ConsCell(goal, curNode.m_callList.m_tail)));
//       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(goal, curNode.m_callList.m_tail));
//       	curNode.m_callList = new ConsCell(goal, curNode.m_callList.m_tail);


//       	System.out.println(curNode.m_callList);
		return true;
	}

	@Override
	public boolean hasMoreChoicePoints()
	{
		return true;//wam != null;
	}

	private final WAM getNewWAM()
    {
		WAM wam = getWAM();
        if(wam instanceof WAMTrace)
            return new WAMTrace((WAMTrace)wam);
        else
            return new WAM(wam);
    }



}
