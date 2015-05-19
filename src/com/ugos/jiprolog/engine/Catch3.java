/**
 *
 */
package com.ugos.jiprolog.engine;

import java.util.Enumeration;
import java.util.Hashtable;

import com.ugos.jiprolog.engine.WAM.Node;

/**
 * @author UgoChirico
 *
 */
public class Catch3 extends Call1
{
	@Override
	public boolean unify(final Hashtable<Variable, Variable> varsTbl)
	{
		final WAM wam = getWAM();
		final WAM.Node thisNode = wam.m_curNode;

		final PrologObject catcher = getParam(2);

		PrologObject goal;

		try
		{
			goal = getGoal(getRealTerm(getParam(1)));
		}
		catch(JIPRuntimeException ex)
		{
			if(catcher.unify(ex.getTerm().getTerm().copy(true), varsTbl))
			{
				final PrologObject handler = getGoal(getRealTerm(getParam(3)));
				thisNode.m_altBody = new ConsCell(handler, null);
				return true;
			}
			else
			{
				throw ex;
			}
		}


		wam.addExceptionListener(new ExceptionListener()
		{
			public boolean notifyException(JIPRuntimeException ex)
			{
				if(catcher.unifiable(ex.getTerm().getTerm()))
				{
					catcher.unify(ex.getTerm().getTerm().copy(true), varsTbl);

					WAM.Node curNode = wam.m_curNode;
					while(curNode != null && curNode != thisNode)
            		{
            			if(curNode.m_varTbl != null)
            			{
	                        Enumeration en = curNode.m_varTbl.keys();
	                        while(en.hasMoreElements())
	                            ((Clearable)en.nextElement()).clear();
            			}

	            		// call precedente
	                    curNode = curNode.m_previous;
            		}

					wam.m_curNode = thisNode;

					final PrologObject handler = getGoal(getRealTerm(getParam(3)));

					thisNode.m_altBody = new ConsCell(handler, null);

					return true;
				}
				else
					return false;
			}
//			public WAM.Node notifyException(JIPRuntimeException ex)
//			{
//				if(catcher.unify(ex.exceptionTerm, varsTbl))
//				{
//					WAM.Node curNode = wam.m_curNode;
//					while(curNode != thisNode)
//            		{
//            			if(curNode.m_varTbl != null)
//            			{
//	                        Enumeration en = curNode.m_varTbl.keys();
//	                        while(en.hasMoreElements())
//	                            ((Clearable)en.nextElement()).clear();
//            			}
//
//	            		// call precedente
//	                    curNode = curNode.m_previous;
//            		}
//
//					WAM.Node node = new Node(new ConsCell(handler, null), null, thisNode, thisNode.m_strModule);
//foo(X) :-	Y is X * 2, throw(test(Y)).
//					return node;
//				}
//				else
//					return null;
//			}
		});

		thisNode.m_altBody = new ConsCell(goal, new ConsCell(new BuiltInPredicate("$reh/0", null), null));

		return true;

	}

	@Override
	public boolean hasMoreChoicePoints()
	{
		return false;
	}
}
