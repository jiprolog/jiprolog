/**
 *
 */
package com.ugos.jiprolog.engine;

/**
 * @author UgoChirico
 *
 * A permission error occurs when an attempt to perform a prohibited operation is made. ErrorTerm has the following form: permission_error(Operation, Permission, Culprit) where Operation is the operation which caused the error, Permission the type of the tried permission and Culprit the argument which caused the error. Operation is one of:

access
add_alias
close
create
input
modify
open
output
reposition
and Permission is one of:

binary_stream
flag
operator
past_end_of_stream
private_procedure
source_sink
static_procedure
stream
text_stream
 *
 */
public class JIPPermissionException extends JIPRuntimeException {

	/**
	 *
	 */
	private static final long serialVersionUID = -1401367774721127591L;
	private String operation;
	private String permission;
	private String culprit;

	public JIPPermissionException(String operation, String permission, String culprit, JIPEngine engine)
	{
		this(operation, permission, culprit);
		m_engine = engine;
	}

	public JIPPermissionException(String operation, String permission, String culprit)
	{
		this.operation = operation;
		this.permission = permission;
		this.culprit = culprit;
	}

	@Override
	public JIPTerm getTerm()
	{
		String strTerm = ((m_term != null) ? (m_term.toString()) : ((m_curNode == null) ? ("undefined") : (m_curNode.getGoal().toString())));

    	return getTerm("permission_error(" + operation + ", " + permission + ", " + culprit + ")", strTerm);
	}

}
