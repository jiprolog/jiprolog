/**
 *
 */
package com.ugos.jiprolog.engine;

import java.util.Hashtable;

/**
 * @author UgoChirico
 *
 */
public class CallN extends BuiltIn {

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl)
	{


		PrologObject closure = getParam(1);
		ConsCell params = (ConsCell)getParams().getTail();

		if(closure instanceof Variable)
		{
			closure = ((Variable)closure).getObject();
		}

		PrologObject goal;

		if(closure instanceof Atom)
		{
			goal = new Functor((Atom)closure);
			((Functor)goal).setParams((ConsCell)params);
		}
		else if(closure instanceof Functor)
		{
			goal = (Functor)closure;

			ConsCell params1 = (ConsCell)((Functor)goal).getParams().copy(true);

			Functor goal1 = new Functor(((Functor)goal).getName(), params1);

			goal.unify(goal1, varsTbl);

			ConsCell newParams = ConsCell.append(params1, (ConsCell)params);

			goal1.setParams(newParams);

			goal = goal1;
		}
		else if(closure instanceof List)
		{
		   if(((List)closure).getHeight() + params.getHeight() > 2)
			   throw new JIPExistenceException("procedure", (new Functor("./3", null)).getPredicateIndicator());
		   goal = new List(closure, params);
  	  	}
		else if(closure instanceof ConsCell)
		{
		   if(((ConsCell)closure).getHeight() + params.getHeight() > 2)
			   throw new JIPExistenceException("procedure", (new Functor("(,)/3", null)).getPredicateIndicator());

		   goal = new ConsCell(closure, params);
		}
        else if(closure == null)
		{
        	throw new JIPParameterUnboundedException();
		}
		else
		{
			throw new JIPTypeException(JIPTypeException.CALLABLE, closure);
		}

//		boolean succeeds = false;
//		WAM wam = getNewWAM();
//		succeeds = wam.query(goal);
//
//		wam.closeQuery();
//
//		return succeeds;
//        if(BuiltInFactory.isBuiltIn(goal.getName()))
//            goal = new BuiltInPredicate(goal);

        // estrae il nodo corrente
        final WAM.Node curNode = getWAM().getCurNode();

       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(goal, curNode.m_callList.m_tail));

		return true;
	}

	@Override
	public boolean hasMoreChoicePoints() {
		return false;
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
