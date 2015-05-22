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
public class JIPPermissionException extends JIPRuntimeException
{
	private static final long serialVersionUID = -1401367774721127591L;
	private String operation;
	private String permission;
	private PrologObject culprit;

	JIPPermissionException(String operation, String permission, PrologObject culprit, JIPEngine engine)
	{
		this(operation, permission, culprit);
		m_engine = engine;
	}

	public JIPPermissionException(String operation, String permission, JIPTerm culprit, JIPEngine engine)
	{
		this(operation, permission, culprit.getTerm());
		m_engine = engine;
	}

	JIPPermissionException(String operation, String permission, PrologObject culprit)
	{
		this.operation = operation;
		this.permission = permission;
		this.culprit = culprit;
	}

	public JIPPermissionException(String operation, String permission, JIPTerm culprit)
	{
		this.operation = operation;
		this.permission = permission;
		this.culprit = culprit.getTerm();
	}

	public JIPPermissionException(String operation, String permission, String culprit)
	{
		this(operation,permission, JIPAtom.create(culprit));
	}

	@Override
	public JIPTerm getTerm()
	{
    	return getTerm(new Functor("permission_error/3", new ConsCell (Atom.createAtom(operation), new ConsCell(Atom.createAtom(permission), new ConsCell(culprit, null)))));

//    	return getTerm("permission_error(" + operation + ", " + permission + ", " + culprit + ")", strTerm);
	}

}
