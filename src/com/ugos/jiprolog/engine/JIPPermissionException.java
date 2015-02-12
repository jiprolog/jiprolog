/**
 *
 */
package com.ugos.jiprolog.engine;

/**
 * @author UgoChirico
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
