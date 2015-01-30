package com.ugos.jiprolog.engine;

import java.lang.management.ManagementFactory;
import java.util.Hashtable;

public class Pid1 extends BuiltIn
{

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl)
	{
		try
		{
			String nameOfRunningVM = ManagementFactory.getRuntimeMXBean().getName();
		    int p = nameOfRunningVM.indexOf('@');

		    String pid = nameOfRunningVM.substring(0, p);

		    return getParam(1).unify(Atom.createAtom(pid), varsTbl);
		}
		catch(Throwable t)
		{
			return false;
		}
	}
}
