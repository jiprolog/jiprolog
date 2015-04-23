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

	WAM m_wam;

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
        	throw new JIPParameterUnboundedException();
		}
		else if(!(param instanceof Atom || param instanceof ConsCell))
		{
			throw new JIPTypeException(JIPTypeException.CALLABLE, param);
		}


		boolean succeeds = false;
		m_wam = getNewWAM();
		succeeds = m_wam.query(param);

		m_wam.closeQuery();

		return succeeds;



//		Functor openSnip = new Functor("<!/0", null);
//
//		BuiltInPredicate closeSnip = new BuiltInPredicate("!>/0", null);
//
//        // estrae il nodo corrente
//        final WAM.Node curNode = getWAM().getCurNode();
//
//       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(openSnip, new ConsCell(goal, new ConsCell(closeSnip, curNode.m_callList.m_tail))));

//		BuiltInPredicate softCut = new BuiltInPredicate("$!/0", null);
//
//        // estrae il nodo corrente
//        final WAM.Node curNode = getWAM().getCurNode();
//
//       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(softCut, new ConsCell(goal, curNode.m_callList.m_tail)));
//       	curNode.m_callList = new ConsCell(curNode.m_callList.m_head, new ConsCell(goal, curNode.m_callList.m_tail));
//       	curNode.m_callList = new ConsCell(goal, curNode.m_callList.m_tail);


//       	System.out.println(curNode.m_callList);
//		return true;
	}

	@Override
	public boolean hasMoreChoicePoints()
	{
		return m_wam == null;
	}

	private final WAM getNewWAM()
    {
        if(getWAM() instanceof WAMTrace)
            return new WAMTrace((WAMTrace)getWAM());
        else
            return new WAM(m_jipEngine);
    }



}
