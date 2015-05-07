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

	WAM wam;

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl)
	{
		PrologObject closure = getParam(1);
		ConsCell params = (ConsCell)getParams().getTail();

		if(closure instanceof Variable)
			closure = ((Variable)closure).getObject();

		PrologObject goal;

		if(closure instanceof Atom)
		{
			if(((Atom)closure).equals(Atom.createAtom(",")))
			{
				goal = (ConsCell)params;
			}
			else
			{
				goal = new Functor((Atom)closure);
				((Functor)goal).setParams((ConsCell)params);

				if(BuiltInFactory.isBuiltIn(((Functor)goal).getName()))
					goal = new BuiltInPredicate(((Functor)goal));
			}
		}
		else if(closure instanceof Functor)
		{
			Atom functorName = ((Functor)closure).getAtom();

			if(functorName.equals(Atom.createAtom(",/1")))
			{
				goal = (Functor)closure;

				ConsCell params1 = (ConsCell)((Functor)closure).getParams().copy(true);

				((Functor)closure).getParams().unify(params1, varsTbl);

				goal = ConsCell.append(params1, (ConsCell)params);
			}
			else
			{
				goal = (Functor)closure;

				ConsCell params1 = (ConsCell)((Functor)goal).getParams().copy(true);

				Functor goal1 = new Functor(((Functor)goal).getName(), params1);

				goal.unify(goal1, varsTbl);

				ConsCell newParams = ConsCell.append(params1, (ConsCell)params);

				goal1.setParams(newParams);

				goal = goal1;

		        if(BuiltInFactory.isBuiltIn(((Functor)goal).getName()))
		            goal = new BuiltInPredicate(((Functor)goal));
			}
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
        	throw new JIPInstantiationException();
		}
		else
		{
			throw new JIPTypeException(JIPTypeException.CALLABLE, closure);
		}

//		if(wam == null)
//		{
//			wam = getNewWAM();
//			if(wam.query(goal))
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


        // estrae il nodo corrente
        final WAM.Node curNode = getWAM().getCurNode();

       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(goal, curNode.m_callList.m_tail));

		return true;
	}

	@Override
	public boolean hasMoreChoicePoints() {
		return wam != null;
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
