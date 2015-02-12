/**
 *
 */
package com.ugos.jiprolog.engine;

/**
 * @author UgoChirico
 *
 */
public class JIPEvaluationException extends JIPRuntimeException {

	/**
	 *
	 */
	public static final String float_overflow = "float_overflow";
	public static final String int_overflow  = "int_overflow";
	public static final String undefined = "undefined";
	public static final String underflow = "underflow";
	public static final String zero_divisor = "zero_divisor";

	private String error;

	public JIPEvaluationException(String error)
	{
		this.error = error;
	}

	@Override
	public JIPTerm getTerm()
	{
		String strTerm = ((m_term != null) ? (m_term.toString()) : ((m_curNode == null) ? ("undefined") : (m_curNode.getGoal().toString())));

    	return getTerm("evaluation_error(" + error + ")", strTerm);
	}

}
