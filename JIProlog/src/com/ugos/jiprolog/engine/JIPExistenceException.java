/**
 *
 */
package com.ugos.jiprolog.engine;

/**
 * @author Ugo Chirico
 * an existence error occurs when an object on which an operation is to be performed does not exist. ErrorTerm has the following form: existence_error(Object, Culprit) where Object is the type of the object and Culprit the argument which caused the error. Object is one of:
procedure
source_sink
stream
 *
 */
public class JIPExistenceException extends JIPRuntimeException {

	/**
	 *
	 */
	private static final long serialVersionUID = 183668069833686724L;
	private String object;
	private PrologObject culprit;

	static JIPExistenceException createSourceSynkException(PrologObject culprit)
	{
		return new JIPExistenceException("source_sink", culprit);
	}

	public static JIPExistenceException createSourceSynkException(String culprit)
	{
		return new JIPExistenceException("source_sink", Atom.createAtom(culprit));
	}

	public static JIPExistenceException createSourceSynkException(JIPTerm culprit)
	{
		return new JIPExistenceException("source_sink", culprit.getTerm());
	}

	static JIPExistenceException createStreamException(PrologObject culprit)
	{
		return new JIPExistenceException("stream", culprit);
	}

	public static JIPExistenceException createStreamException(String culprit)
	{
		return new JIPExistenceException("stream", Atom.createAtom(culprit));
	}

	public static JIPExistenceException createStreamException(JIPTerm culprit)
	{
		return new JIPExistenceException("stream", culprit.getTerm());
	}

	public static JIPExistenceException createProcedureException(JIPTerm culprit)
	{
		return new JIPExistenceException("procedure", culprit.getTerm());
	}

	static JIPExistenceException createProcedureException(PrologObject culprit)
	{
		return new JIPExistenceException("procedure", culprit);
	}

	JIPExistenceException(String object, PrologObject culprit)
	{
		this.object = object;
		this.culprit = culprit;
	}

	@Override
	public JIPTerm getTerm()
	{
    	return getTerm(new Functor("existence_error/2", new ConsCell (Atom.createAtom(object), new ConsCell(culprit,  null))));

//    	return getTerm("existence_error(" + object + ", " + Atom.createAtom(culprit) + ")", strTerm);
	}

}
