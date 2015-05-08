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

//	WAM wam;

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl)
	{
		PrologObject param = getParam(1);
		if(param instanceof Variable)
		{
			param = ((Variable)param).getObject();
		}

		PrologObject goal = getGoal(param);

        // extract the current node (i.e. current choice point)

        final WAM.Node curNode = getWAM().getCurNode();

        curNode.m_altBody = new ConsCell(param, null);

		return true;

		/*
		if(wam == null)
		{
			wam = getNewWAM();
			if(wam.query(param))
				return true;
			else
			{
				wam.closeQuery();
				wam = null;
			}
		}
		else
		{
			if(wam.nextSolution())
				return true;
			else
			{
				wam.closeQuery();
				wam = null;
			}

		}

		return false;
*/

	}

	@Override
	public boolean hasMoreChoicePoints()
	{
		return false;//wam != null;
	}

	protected PrologObject getGoal(PrologObject param)
	{
		PrologObject goal;

		if(param instanceof Atom)
		{
			goal = new Functor((Atom)param);

			if(BuiltInFactory.isBuiltIn(((Functor)goal).getName()))
				goal = new BuiltInPredicate(((Functor)goal));
		}
		else if(param instanceof Functor)
		{
			goal = (Functor)param;

	        if(BuiltInFactory.isBuiltIn(((Functor)goal).getName()))
	            goal = new BuiltInPredicate(((Functor)goal));
		}
		else if(param instanceof List)
		{
		   goal = param;
  	  	}
		else if(param instanceof ConsCell)
		{
			// TODO wrapping naked vars
		   goal = param;

		   Clause.checkForCallable((ConsCell)goal);
		}
        else if(param == null)
		{
        	throw new JIPInstantiationException();
		}
		else
		{
			throw new JIPTypeException(JIPTypeException.CALLABLE, param);
		}

		return goal;
	}

//	private final WAM getNewWAM()
//    {
//		WAM wam = getWAM();
//        if(wam instanceof WAMTrace)
//            return new WAMTrace((WAMTrace)wam);
//        else
//            return new WAM(wam);
//    }



}
