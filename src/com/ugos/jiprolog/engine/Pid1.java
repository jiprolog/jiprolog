package com.ugos.jiprolog.engine;

import java.lang.reflect.Method;
import java.util.Hashtable;

public class Pid1 extends BuiltIn
{

	@Override
	public boolean unify(Hashtable<Variable, Variable> varsTbl)
	{
		try
		{
			Class clazz = Class.forName("java.lang.management.ManagementFactory");
			Class clazz1 = Class.forName("java.lang.management.RuntimeMXBean");

			if(clazz != null)
			{
				Method getRuntimeMXBeanMethod = clazz.getMethod("getRuntimeMXBean");
				Object runtimeMXBean = getRuntimeMXBeanMethod.invoke(null);

				Method getNameMethod = clazz1.getDeclaredMethod("getName");

				String nameOfRunningVM = (String)getNameMethod.invoke(runtimeMXBean);

				int p = nameOfRunningVM.indexOf('@');

				String pid = nameOfRunningVM.substring(0, p);

				return getParam(1).unify(Expression.createNumber(pid), varsTbl);
			}

			return false;
		}
		catch(Throwable t)
		{
			t.printStackTrace();
			return false;
		}
	}
}
