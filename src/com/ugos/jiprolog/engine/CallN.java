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

		Functor goal;

		if(closure instanceof Atom)
		{
			goal = new Functor((Atom)closure);
			goal.setParams((ConsCell)params);
		}
		else if(closure instanceof Functor)
		{
			goal = (Functor)closure;

			ConsCell params1 = (ConsCell)goal.getParams().copy(true);

			Functor goal1 = new Functor(goal.getName(), params1);

			goal.unify(goal1, varsTbl);

//			if(!goal.unify(goal1, varsTbl))
//				System.out.println("not unify");


			ConsCell newParams = ConsCell.append(params1, (ConsCell)params);

			goal1.setParams(newParams);

			goal = goal1;
//			goal.setParams(newParams);
		}
        else if(closure == null)
		{
        	throw new JIPParameterUnboundedException();
		}
		else
		{
			throw new JIPTypeException(JIPTypeException.CALLABLE, closure);
		}

        if(BuiltInFactory.isBuiltIn(goal.getName()))
            goal = new BuiltInPredicate(goal);

        // estrae il nodo corrente
        final WAM.Node curNode = getWAM().getCurNode();

       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(goal, curNode.m_callList.m_tail));

		return true;
	}

	@Override
	public boolean hasMoreChoicePoints() {
		return false;
	}



}
