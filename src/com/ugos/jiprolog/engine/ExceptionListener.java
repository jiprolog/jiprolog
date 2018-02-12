package com.ugos.jiprolog.engine;

public interface ExceptionListener {
//	public WAM.Node notifyException(JIPRuntimeException ex);
	public boolean notifyException(JIPRuntimeException ex);
	public Catch3 getCallingTerm();
}