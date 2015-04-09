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
	private PrologObject culprit;

	JIPRepresentationException(String typeError, PrologObject culprit)
	{
       	this.typeError = typeError;

		this.culprit = culprit;
	}

	JIPRepresentationException(String typeError, PrologObject culprit, JIPEngine engine)
	{
		this(typeError, culprit);
		m_engine = engine;
	}

	@Override
	public JIPTerm getTerm()
	{
    	return getTerm(new Functor("representation_error/1", new ConsCell (Atom.createAtom(typeError), null)));

//    	return getTerm("type_error(" + typeError + ", " + culprit + ")", strTerm);
	}

}
