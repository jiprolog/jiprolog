/**
 *
 */
package com.ugos.jiprolog.engine;

import java.util.Hashtable;

/**
 * @author UgoChirico
 *
 */
public class SaveCP1 extends BuiltIn {

//	WAM wam;

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl)
	{
		PrologObject param = getParam(1);

        // extract the current node (i.e. current choice point)
        final WAM.Node curNode = getWAM().getCurNode();

		return param.unify(Expression.createNumber(curNode.hashCode()), varsTbl);
	}

	@Override
	public boolean hasMoreChoicePoints()
	{
		return false;//wam != null;
	}
}
