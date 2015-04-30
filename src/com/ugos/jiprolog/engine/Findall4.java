package com.ugos.jiprolog.engine;

import java.util.Hashtable;
import java.util.Stack;

final class Findall4 extends BuiltIn
{
    WAM m_wam;
    PrologObject m_res;
    PrologObject m_solList;

    public final boolean unify(final Hashtable varsTbl)
    {
        PrologObject term  = getParam(1);
        PrologObject query = getRealTerm(getParam(2));
        PrologObject list  = getRealTerm(getParam(3));
        PrologObject tail  = getRealTerm(getParam(4));

        if(query == null)
            throw new JIPInstantiationException(2);

		if(list != null)
		{
			if(!(list instanceof List))
				throw new JIPTypeException(JIPTypeException.LIST, list);
			if(!(((ConsCell)list).isClosedOrPartial()))
				throw new JIPTypeException(JIPTypeException.LIST, list);
		}

		if(tail != null)
		{
			if(!(tail instanceof List))
				throw new JIPTypeException(JIPTypeException.LIST, tail);
			if(!(((ConsCell)tail).isClosedOrPartial()))
				throw new JIPTypeException(JIPTypeException.LIST, tail);
		}

        m_res = getParam(3);

        m_wam = getNewWAM();

        Stack<PrologObject> solStack = new Stack<PrologObject>();

        if(m_wam.query(new ConsCell(query, null)))
        {
            solStack.push(term.copy(false));
            while(m_wam.nextSolution())
            {
                solStack.push(term.copy(false));
            }
        }


        tail = getParam(4);

        m_solList = tail;

		while(!solStack.isEmpty())
        {
            m_solList = new List((PrologObject)solStack.pop(), m_solList);
        }

        m_wam.closeQuery();

//        term.clear();

        return m_res.unify(m_solList, varsTbl);
    }

    public final boolean isDeterministic()
    {
        return m_wam == null; // never started
    }

    private final WAM getNewWAM()
    {
        if(getWAM() instanceof WAMTrace)
            return new WAMTrace((WAMTrace)getWAM());
        else
            return new WAM(m_jipEngine);
    }
}
