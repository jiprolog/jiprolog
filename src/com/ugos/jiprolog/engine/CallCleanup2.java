package com.ugos.jiprolog.engine;

import java.util.Hashtable;

public class CallCleanup2 extends Call1 {

	WAM wam;
	private PrologObject m_goal;
	private PrologObject m_cleanupGoal;

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl) {

		if(m_goal == null)
		{
			PrologObject goal1 = getParam(1);
			if(goal1 instanceof Variable)
			{
				goal1 = ((Variable)goal1).getObject();
			}

			m_goal = getGoal(goal1).copy(true);
		}

		if(m_cleanupGoal == null)
		{
			PrologObject goal1 = getParam(2);
			if(goal1 instanceof Variable)
			{
				goal1 = ((Variable)goal1).getObject();
			}

			m_cleanupGoal = getGoal(goal1).copy(true);
		}

		boolean solution = false;
		JIPRuntimeException exception = null;

		if(wam == null)
		{
			wam = getNewWAM();
			try
			{
				solution = wam.query(m_goal);
				if(solution)
				{
					getParam(1).unify(m_goal.copy(true), varsTbl);

					if(wam.hasMoreChoicePoints())
						return true;
				}
			}
			catch(JIPRuntimeException ex)
			{
				exception  = ex;
			}
		}
		else
		{
			try
			{
				solution = wam.nextSolution();
				if(solution)
				{
					getParam(1).unify(m_goal.copy(true), varsTbl);
					if(wam.hasMoreChoicePoints())
						return true;
				}

			}
			catch(JIPRuntimeException ex)
			{
				exception = ex;
			}
		}

		callCleanup(varsTbl);

		if(exception != null)
			throw exception;
		else
			return solution;
	}

	private void callCleanup(Hashtable<Variable, Variable> varsTbl)
	{
		wam.closeQuery();

		boolean solution = wam.query(new Functor("once/1", new ConsCell(m_cleanupGoal, null)));

		if(solution)
		{
			getParam(2).unify(m_cleanupGoal.copy(true), varsTbl);
		}

		wam.closeQuery();

//        final WAM.Node curNode = getWAM().getCurNode();
//        curNode.m_altBody = new ConsCell(new Functor("once/1", new ConsCell(m_cleanupGoal, null)), null);
	}

	@Override
	public boolean hasMoreChoicePoints()
	{
		return wam != null && !wam.isClosed() && wam.hasMoreChoicePoints();//true;//m_goal1 == null || m_goal2 == null;
	}
}
