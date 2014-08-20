/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the Affero GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Affero GNU General Public License for more details.
 *
 * You should have received a copy of the Affero GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package com.ugos.JIProlog.engine;

/** Exception raised when a built-in predicate find a parameter of unexpected type.
 * @version 3.0
 * @author Ugo Chirico 2005<br>
 * Home Page: http://www.ugochirico.com
 * @see com.ugos.JIProlog.engine.JIPRuntimeException
 */
public class JIPParameterTypeException extends JIPRuntimeException
{
    private static final short ERR_PARMTYPE = 3;
    private String m_strExpected = null;
    private int m_nParam = -1;
    public static final int UNDEFINED = 0;
    public static final int FUNCTOR = 1;
    public static final int ATOM = 2;
    public static final int ATOM_OR_STRING = 3;
    public static final int INTEGER = 4;
    public static final int FLOAT = 5;
    public static final int PREDICATE_INDICATOR = 6;
    public static final int COMPOUND = 7;
    public static final int LIST = 8;
    public static final int COMPARATION_OPERATOR = 9;
    public static final int CALLABLE = 10;
    public static final int NUMERIC_EXPRESSION = 11;
    
    
    private static final String s_stringMap[] = new String[12];
    
    static
    {
        s_stringMap[FUNCTOR] = "functor";
        s_stringMap[ATOM] = "atom";
        s_stringMap[FLOAT] = "float";
        s_stringMap[ATOM_OR_STRING] = "atom_or_string";
        s_stringMap[INTEGER] = "integer";
        s_stringMap[PREDICATE_INDICATOR] = "predicate_indicator";
        s_stringMap[COMPOUND] = "compound";
        s_stringMap[LIST] = "list";
        s_stringMap[UNDEFINED] = "undefined";
        s_stringMap[COMPARATION_OPERATOR] = "comparation_operator";
        s_stringMap[NUMERIC_EXPRESSION] = "numeric_expression";
        s_stringMap[CALLABLE] = "callable";
    }
    
    /** Constructs a new JIPParameterTypeException
     * @param nParam the index of the bad parameter
     * @param nExpectedType the type of the expected term
     */
    public JIPParameterTypeException(final int nParam, final int nExpectedType)
    {
        super(create(3, (nParam > -1 ? new Integer(nParam) : null)));
        
        if(nExpectedType < UNDEFINED && nExpectedType > CALLABLE)
            m_strExpected = s_stringMap[UNDEFINED];    
        else
            m_strExpected = s_stringMap[nExpectedType];
        
        m_nParam = nParam;
    }
    
    /** Constructs a new JIPParameterTypeException
     */
    public JIPParameterTypeException()
    {
        super(create(3, null));
        m_strExpected = s_stringMap[UNDEFINED];
    }
    
    /** Gets the error term associated to this exception
     * @return JIPTerm object associated to this exception
     */
    public JIPTerm getTerm()
    {
        String strTerm = ((m_term != null) ? (m_term.toString()) : ((m_curNode == null) ? ("undefined") : (m_curNode.getGoal().toString())));
        
        return getTerm("type_error(" + Atom.createAtom(m_strExpected).toString() + ")", strTerm);
        
        
//        if(m_engine == null)
//            return super.getTerm();
//        
//        String strTerm = ((m_term != null) ? (m_term.toString()) : ((m_curNode == null) ? ("undefined") : (m_curNode.getGoal().toString()))); 
//        try
//        {
//            return m_engine.getTermParser().parseTerm("error(type_error(" + m_strExpected + "), context('" + strTerm + "', file('" + m_strFileName + "', " + m_nLineNumber + ")))");
//        }
//        catch (JIPSyntaxErrorException ex)
//        {
//             return m_engine.getTermParser().parseTerm("error(type_error(" + m_strExpected + "), context(undefined, file(undefined, undefined)))");
//        }
        
/*        
        String strError = "error(type_error(" + m_strExpected + "), context(" + super.getTerm() + ", ";
        if(m_nParam != -1)
            strError += m_nParam;
        else
            strError += "one_of_the_arguments";
        
        strError += "))";
        
        return m_engine.getTermParser().parseTerm(strError);
  */
    }
}
