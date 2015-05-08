/**
 *
 */
package com.ugos.jiprolog.engine;

import java.util.Hashtable;

/**
 * @author UgoChirico
 *
 */
public class Disjunction2 extends Call1 {

//	WAM wam;

	private PrologObject m_goal1;
	private PrologObject m_goal2;
	private boolean end = false;
	private boolean ifthenelse = false;
	private boolean starifthenelse = false;

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl)
	{
		PrologObject goal;

		if(m_goal1 == null)
		{
			PrologObject goal1 = getParam(1);
			if(goal1 instanceof Variable)
			{
				goal1 = ((Variable)goal1).getObject();
			}

			m_goal1 = goal = getGoal(goal1);
		}
		else if(m_goal2 == null)
		{
			PrologObject goal2 = getParam(2);
			if(goal2 instanceof Variable)
			{
				goal2 = ((Variable)goal2).getObject();
			}

			m_goal2 = goal = getGoal(goal2);
		}
		else
		{
			end = true;
			return false;
		}

        // extract the current node (i.e. current choice point)
        final WAM.Node curNode = getWAM().getCurNode();

        if(m_goal2 != null)
        {
        	if(ifthenelse)
        	{
//				;(->(_X,_Y), Z) :- !, Z.
				curNode.m_altBody = new ConsCell(new BuiltInPredicate(Atom.createAtom("!/0"), null), new ConsCell(goal, null));
        	}
        	else if(starifthenelse)
        	{
//				;(*->(_X,_Y), Z) :- Z.
				curNode.m_altBody = new ConsCell(goal, null);
        	}
        	else
        	{
        		// transparent cut
        		curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(goal, ((ConsCell)curNode.m_callList.m_tail).getTail()));
        	}
        }
        else
        {
        	// manage ->(X,Y)
			if(goal instanceof Functor)
			{
				if(((Functor)goal).getAtom().equals(Atom.IF))
				{
					ifthenelse = true;

					ConsCell funparams = ((Functor)goal).getParams();

					// ->(X,Y) :- call(X), !, Y.
					curNode.m_altBody = new ConsCell(new BuiltInPredicate(Atom.createAtom("call/1"), new ConsCell(funparams.m_head, null)), new ConsCell(new BuiltInPredicate(Atom.createAtom("!/0"), null), new ConsCell(funparams.getTerm(2), null)));
				}
				else if(((Functor)goal).getAtom().equals(Atom.STARIF))
				{
					starifthenelse = true;

					// *->(X,Y) :- X, Y.
					curNode.m_altBody = ((Functor)goal).getParams();
				}
				else
				{
					// transparent cut
					curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(goal, curNode.m_callList.m_tail));
				}
			}
			else
			{
				// transparent cut
				curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(goal, curNode.m_callList.m_tail));
			}
        }

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
		return !end;//true;//m_goal1 == null || m_goal2 == null;
	}
}
