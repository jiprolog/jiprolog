/**
 *
 */
package com.ugos.jiprolog.engine;

/**
 * @author UgoChirico
 *

 */
public class JIPRepresentationException extends JIPRuntimeException {

	/**
	 *
	 */

	private String typeError;

	public JIPRepresentationException(String typeError)
	{
       	this.typeError = typeError;
	}


	JIPRepresentationException(String typeError, JIPEngine engine)
	{
		this(typeError);
		m_engine = engine;
	}

	@Override
	public JIPTerm getTerm()
	{
    	return getTerm(new Functor("representation_error/1", new ConsCell (Atom.createAtom(typeError), null)));

//    	return getTerm("type_error(" + typeError + ", " + culprit + ")", strTerm);
	}

}
