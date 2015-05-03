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

package com.ugos.jiprolog.engine;

import java.io.*;
import java.util.*;

final class PrologParser
{
    private static final int STATE_NONE           = 0;
    private static final int STATE_ROUND_BRACKET  = 1;
    private static final int STATE_SQUARE_BRACKET = 2;
    private static final int STATE_PIPE           = 3;
    private static final int STATE_ARG_LIST       = 4;
    private static final int STATE_SPECIAL_BRACKET = 5;

    private static String CASE_CHARS = "()[]{}.|";

    private Hashtable<String, Variable> m_varTable = new Hashtable<String, Variable>(10);
    private Hashtable<String, Variable> m_singVarTable = new Hashtable<String, Variable>(10);

    private PrologTokenizer m_tokenizer;

    // serve solo per le parentesi quadre nel caso [(a,b)]
//    private int m_nTokCount;

    //private ParserInputStream m_lnReader;
    private ParserReader m_lnReader;
    private OperatorManager m_opManager;
    private JIPEngine m_engine;
    private String m_strFileName;

    private String sign = "";


//    public static void main(String[] args)
//    {
//////        //StringReader reader = new StringReader(args[0]);
//////        //StringReader reader = new StringReader("atomo.");
//////        //StringReader reader = new StringReader("(atomo).");
//////        //StringReader reader = new StringReader("(atomo, ugo).");
//////        //StringReader reader = new StringReader("[r, f,u].");
//////        //StringReader reader = new StringReader("[atomo, -, b,d,r,f,r].");
//////        //StringReader reader = new StringReader("(atomo, [ugo, ornella]).");
//////        //StringReader reader = new StringReader("[atomo, [ugo, ornella]].");
//////        //StringReader reader = new StringReader("ugo(ornella, atomo).");
//////        //StringReader reader = new StringReader("ugo(_, _).");
//////        //StringReader reader = new StringReader("ugo(ornella, [atomo, a, -]).");
//////        //StringReader reader = new StringReader("ugo(ornella, [-]).");
//////        //StringReader reader = new StringReader("ugo(ornella, (-)).");
//////        //StringReader reader = new StringReader("ugo(ornella, [atomo, a, b, c, -]).");
//////        //StringReader reader = new StringReader("ugo([ornella, atomo]).");
//////        //StringReader reader = new StringReader("-(ornella, ugo, asdfg).");
//////        //StringReader reader = new StringReader("!(ornella, ugo, asdfg).");
//////        //StringReader reader = new StringReader("(5 - (3 * 6)).");
//////        //StringReader reader = new StringReader("ornella:-ugo.");
//////        //StringReader reader = new StringReader("ornella:-ugo(X).");
//////        //StringReader reader = new StringReader(":-ugo(X).");
//////        //StringReader reader = new StringReader("ornella(X,Y):-ugo(X, :-, Z), uyt(Z).");
//////        //StringReader reader = new StringReader("ornella(X,Y):-ugo(X, \"ugo\"; Z), uyt(Z) ; jh.");
//////        //StringReader reader = new StringReader("(a, b ; c).");
//////        //StringReader reader = new StringReader("ornella(X,Y):-ugo(X, 'ugo''s', Z), uyt(Z).");
//////        //StringReader reader = new StringReader("ornella(X,Y):-ugo(X, \"ugo~M 2\", Z), uyt(Z).");
//////        //StringReader reader = new StringReader("\"ugo~M 2\".");
//////        //StringReader reader = new StringReader("9.2.");
//////        //StringReader reader = new StringReader("3 * -9.2E+10.");
//////
//////        //StringReader reader = new StringReader("/* 3 * -9.2E+10.\r\nugo vhirico*/");
//////        //StringReader reader = new StringReader("%3 * -9.2E+10ugo vhirico*/");
//////        //StringReader reader = new StringReader("N is N1 + 1.");
//////        //StringReader reader = new StringReader("(a,b;c,d");
//////        //StringReader reader = new StringReader("X is 3 - 2, ab");
//////        //StringReader reader = new StringReader("test(N, List, RevList).");
//////        //StringReader reader = new StringReader("2 * (3 + - 2).");
//////
//////        //StringReader reader = new StringReader("X ; Y :- X, Y.");
//////        //StringReader reader = new StringReader("X =.. [a,b,c,d] ; X =.. [e,f,g,h] ; X =.. [i,l,m,n].");
//////        //StringReader reader = new StringReader("a ; b ; c.");
//////
//////        //StringReader reader = new StringReader("circle(C) class ellipse checks none body all => (a, b).");
//////        //StringReader reader = new StringReader("pi(3.14567).");
//////        //StringReader reader = new StringReader("a(b:-c,+,i).");
//////        //StringReader reader = new StringReader("module:ugo(X):-ornella(X).");
//////        //StringReader reader = new StringReader("subclass_translations :- ( SubCDesc class SuperCName checks Conds body Body ), SubCDesc =.. [ SubCName|Args ].");
//////        //StringReader reader = new StringReader("subclass_translations :- ( SubCDesc class SuperCName checks Conds body Body ), SubCDesc =.. [ SubCName|Args ], assert_once( (SubCCall :- SuperCCall) ), assert_once( (class SubCDesc checks (instance(SuperCDesc), Conds) body Body) ), fail.");
//////        //StringReader reader = new StringReader("assert_predicates( CName, P1 -&- P2, Args ) :- assert_predicates( CName, P1, Args ),assert_predicates( CName, P2, Args ).");
//////        //StringReader reader = new StringReader("assert_predicates( _, none, _ ) :- !.");
//////
//////        //StringReader reader = new StringReader("( a:-b )");
//////        //StringReader reader = new StringReader("dcg_and(In1, In2, (In1, ((a, b))), X).");
//////        //StringReader reader = new StringReader("({ write(ugo) }, [boy], [house]).");
//////        //StringReader reader = new StringReader("ugo((a,b,c)).");
//////        //StringReader reader = new StringReader(":-(a, b, c, d) , (X, Y).");
//////        //StringReader reader = new StringReader("({a,b}, c,d).");
//////        //StringReader reader = new StringReader("assert_once( (class SubCDesc checks (instance(SuperCDesc), Conds) body Body) ).");
////////        StringBufferInputStream  reader = new StringBufferInputStream ("ugo.'a'(:- op(900, xfx, ugo), X).");
////////      StringBufferInputStream reader = new StringBufferInputStream("write('.').");
////      //           StringBufferInputStream reader = new StringBufferInputStream("X = .(a, .(b, [])).");
////
//        //          StringBufferInputStream reader = new StringBufferInputStream("write('|')");
//            //StringBufferInputStream reader = new StringBufferInputStream("(((a,b), c, (d,e)))");
//        //StringBufferInputStream reader = new StringBufferInputStream("[a,(d,e)]");
//      //StringBufferInputStream reader = new StringBufferInputStream("(a, (d,e))");
//      //StringBufferInputStream reader = new StringBufferInputStream("a - -, b");
        //      StringBufferInputStream reader = new StringBufferInputStream("/ / ',' a");
//        StringBufferInputStream reader = new StringBufferInputStream("a:-(b,c),d.");
//
////
////
////////
////////        //ugo((a,b,c)).
////////
////////
//        try
//        {
//            //FileReader reader = new FileReader("com\\ugos\\JIProlog\\resources\\JIPKernel.txt");
//            PrologParser parser = new PrologParser(new ParserInputStream(reader), new OperatorManager());
//            PrologObject obj;
//            while((obj = parser.parseNext()) != null)
//            {
//                System.out.println(obj);
//                System.out.println(obj.getClass());
//
//            }
//            //            System.out.println(obj.getClass());
//        }
//        catch(Exception ex)
//        {
//            ex.printStackTrace();
//        }
//    }

    PrologParser(final ParserReader reader, final OperatorManager opManager, final JIPEngine engine, final String strFileName)
    {
        m_lnReader = reader;//new LineNumberReader(reader);
        m_tokenizer = new PrologTokenizer(m_lnReader, strFileName);
        m_opManager = opManager;
        m_engine = engine;
        m_strFileName = strFileName;
    }

    PrologObject parseNext() throws JIPSyntaxErrorException
    {
//      System.out.println("parseNext");
        m_varTable.clear();
        m_singVarTable.clear();

        return translateTerm(STATE_NONE, m_lnReader);
    }

    Hashtable<String, Variable> getSingletonVariables()
    {
        return m_singVarTable;
    }

    final private PrologObject translateTerm(int nState, ParserReader lnReader) throws JIPSyntaxErrorException
    {
        Stack<Object> termStack = new Stack<Object>();
        boolean bEnd = false;
        boolean bWhiteSpace = false;
        boolean lastParenthesis = false;
        boolean quoted = false;
        boolean started = false;

        PrologTokenizer.Token tok = null;

        int line = lnReader.getLineNumber();
    	int column = lnReader.getColumn();
    	int position = lnReader.getRead();

        try
        {
            while(!bEnd)
            {
                tok = m_tokenizer.getNextToken();
                if(tok == null)
                {
                    break;
                }

                if(tok.m_nType != PrologTokenizer.TOKEN_WHITESPACE)
                {
                	if(!started)
                	{
                		line = lnReader.getLineNumber();
                		column = lnReader.getColumn();
                		position = lnReader.getRead();
                		started = true;
                	}
                }

//              System.out.println("nextoken " + tok.m_strToken);
                Object lastObj = null;

                if(!termStack.isEmpty())
                    lastObj = termStack.peek();

                switch(tok.m_nType)
                {
                    case PrologTokenizer.TOKEN_NUMBER:
                        {
                            Expression exp;
                            if(lastObj instanceof PrologObject)
                                throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "operator_expected(" + ((PrologObject)lastObj).toString(m_opManager) + ")");

                            exp = Expression.createNumber(sign + tok.m_strToken);

                            sign = "";

                            termStack.push(exp);

                            lastParenthesis = false;
                        }
                        break;

                    case PrologTokenizer.TOKEN_VARIABLE:
                        {
                            Variable var;

                            if(tok.m_strToken.equals("_"))
                            {
                                var = new Variable(true);
                            }
                            else
                            {
                                if(m_varTable.containsKey(tok.m_strToken))
                                {
                                    m_singVarTable.remove(tok.m_strToken);
                                    var = (Variable)m_varTable.get(tok.m_strToken);
                                }
                                else
                                {
                                    var = new Variable(tok.m_strToken);
                                    m_varTable.put(tok.m_strToken, var);
                                    if(!(tok.m_strToken.startsWith("_")))
                                        m_singVarTable.put(tok.m_strToken, var);
                                }
                            }

                            termStack.push(var);

                            lastParenthesis = false;
                        }
                        break;

                    case PrologTokenizer.TOKEN_DBLQUOTE:
                        {
                            if(lastObj instanceof PrologObject)
                                throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "operator_expected(" + ((PrologObject)lastObj).toString(m_opManager) + ")");

                            String strVal = tok.m_strToken.substring(1, tok.m_strToken.length() - 1);

                            if(strVal.length() == 0)
                            {
                            	String double_quotes = (String)m_engine.getEnvVariable("double_quotes");
                            	if("atom".equals(double_quotes))
                            	{
                            		termStack.push(Atom.createAtom(""));
                            	}
                            	else
                            	{
                            		termStack.push(List.NIL);
                            	}
                            }
                            else
                            {
                            	String double_quotes = (String)m_engine.getEnvVariable("double_quotes");
                            	if("atom".equals(double_quotes))
                            	{
                            		termStack.push(Atom.createAtom(strVal));
                            	}
                            	else if("chars".equals(double_quotes))
                            	{
                                    termStack.push(new PString(strVal, true));

                            	}
                            	else if("codes".equals(double_quotes))
                            	{
                            		termStack.push(new PString(strVal, false));
                            	}
                            	else
                            	{
                                    throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "invalid_option(double_quotes(" + double_quotes + "))");
                            	}

                            }

                            lastParenthesis = false;
                        }
                        break;

                    case PrologTokenizer.TOKEN_QUOTE:
                        tok.m_strToken = tok.m_strToken.substring(1, tok.m_strToken.length() - 1);
                    	if(tok.m_strToken.length() == 1 && CASE_CHARS.indexOf(tok.m_strToken.charAt(0)) > -1)
                    	{
                    	    // il quoted atom è un carattere presente nel case e va trattato come atomo
                    	    termStack.push(Atom.createAtom(tok.m_strToken));
                    	    break;
                    	}
                    	quoted = true;

//                    case PrologTokenizer.TOKEN_SIGN:
//                    	sign = tok.m_strToken;
                    case PrologTokenizer.TOKEN_ATOM:
                    case PrologTokenizer.TOKEN_SPECIAL_ATOM:
                    case PrologTokenizer.TOKEN_SINGLETON:
                        {
                            if(tok.m_strToken.equals("."))
                            {
                                if(nState == STATE_NONE && !(lastObj instanceof Operator))  // DOT
                                {
                                    bEnd = true;
                                }
                                else
                                {
                                    termStack.push(Atom.createAtom("."));
                                }

                                lastParenthesis = false;
                            }
                            else if(tok.m_strToken.equals("("))
                            {
                                PrologObject term;

                                lastParenthesis = false;

                                if(lastObj instanceof Atom)// || lastObj instanceof Operator)
                                {
                                    if(bWhiteSpace)
                                        throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_blank_before(" + ((PrologObject)lastObj).toString(m_opManager) + ")");

                                    term = translateTerm(STATE_ARG_LIST, lnReader);

                                    termStack.pop();
                                    if(!(term instanceof ConsCell) || term  instanceof List || term  instanceof Functor)
                                    {
                                        term = new ConsCell(term, null);
                                    }

                                    PrologObject funct = makeFunctor((Atom)lastObj, (ConsCell)term);
                                    termStack.push(funct);

                                }
                                // termStack.size() == 1  indica che lastObj è il primo termine letto e quindi anche se l'operatore è infix va trattato come atomo
                                // bWhiteSpace indica il termine precedente è uno white-space
                                else if(lastObj instanceof Operator && !bWhiteSpace)
                                {
                                    if(termStack.size() == 1 || ((Operator)lastObj).getInfix() == null)
                                    {
                                        //non ci sono altri termini davanti
//                                    System.out.println("lastObj " + lastObj + " linenumber " + (m_lnReader.getLineNumber() + 1));

                                        term = translateTerm(STATE_ARG_LIST, lnReader);

//                                    System.out.println("term " + term);

	                                    termStack.pop();
	                                    if(!(term instanceof ConsCell) || term  instanceof List || term  instanceof Functor)
	                                    {
	                                        term = new ConsCell(term, null);
	                                    }

	                                    PrologObject funct = makeFunctor(Atom.createAtom(((Operator)lastObj).getName()), (ConsCell)term);
	                                    termStack.push(funct);
                                    }
                                    else
                                    {
                                        // last obj è infisso e c'è un antecedente
                                        termStack.pop();
                                        Object lastLastObj = termStack.peek();
                                        termStack.push(lastObj);

                                        if(lastLastObj instanceof Operator &&
                                          ((((Operator)lastLastObj).getPrecedence() > ((Operator)lastObj).getPrecedence()) ||
                                          (((Operator)lastLastObj).getPrecedence() == ((Operator)lastObj).getPrecedence() && !((Operator)lastObj).isNonAssoc() &&
                                          (!((Operator)lastLastObj).isLeftAssoc() || ((Operator)lastObj).isRightAssoc()))))
                                        {
                                            //tratto lastOP come funtore
                                            term = translateTerm(STATE_ARG_LIST, lnReader);

//                                            System.out.println("term " + term);

    	                                    termStack.pop();
    	                                    if(!(term instanceof ConsCell) || term  instanceof List || term  instanceof Functor)
    	                                    {
    	                                        term = new ConsCell(term, null);
    	                                    }

    	                                    PrologObject funct = makeFunctor(Atom.createAtom(((Operator)lastObj).getName()), (ConsCell)term);
    	                                    termStack.push(funct);
                                        }
                                        else
                                        {
//                                          tratto lastop come operatore infisso
                                        	if(lastLastObj instanceof Operator && ((Operator)lastObj).getInfix() != null)
                                			{
                                                //tratto lastOP come funtore
                                                term = translateTerm(STATE_ARG_LIST, lnReader);

//                                                System.out.println("term " + term);

        	                                    termStack.pop();
        	                                    if(!(term instanceof ConsCell) || term  instanceof List || term  instanceof Functor)
        	                                    {
        	                                        term = new ConsCell(term, null);
        	                                    }

        	                                    PrologObject funct = makeFunctor(Atom.createAtom(((Operator)lastObj).getName()), (ConsCell)term);
        	                                    termStack.push(funct);
                                			}
                                			else
                                			{
	                                            term = translateTerm(STATE_ROUND_BRACKET, lnReader);

	                                            lastParenthesis = true;

	//                                            System.out.println("term " + term);
	                                            if (term == null)
	                                            {
	                                                termStack.push(ConsCell.NIL);
	                                            }
	                                            else
	                                            {
	                                                termStack.push(term);
	                                            }
                                			}
                                        }
                                    }
                                }
                                else if(lastObj instanceof PrologObject)
                                {
                                    throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "operator_expected(" + ((PrologObject)lastObj).toString(m_opManager) + ")");
                                }
                                else
                                {
//                                    System.out.println("lastObj2 " + lastObj);
                                    term = translateTerm(STATE_ROUND_BRACKET, lnReader);

                                    lastParenthesis = true;

                                    if (term == null)
                                    {
                                        termStack.push(ConsCell.NIL);
                                    }
                                    else
                                    {
                                        termStack.push(term);
                                    }
                                }
                            }
                            else if(tok.m_strToken.equals(")"))
                            {
                                if(nState == STATE_ROUND_BRACKET || nState == STATE_ARG_LIST)
                                {
                                    if(lastObj instanceof Operator)
                                    {
                                        Operator lastOp = (Operator) termStack.pop();
                                        termStack.push(Atom.createAtom(lastOp.getName()));
                                    }
                                    else if(((lastObj instanceof ConsCell) && !(lastObj instanceof Functor) && !(lastObj instanceof List) &&  nState == STATE_ARG_LIST))
                                    {
                                        termStack.pop();
                                        termStack.push(new ConsCell((ConsCell)lastObj, null));
                                    }

                                    if(termStack.isEmpty())
                                    {
                                        throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "empty_bracket");
                                    }
                                    nState = STATE_NONE;
                                    bEnd = true;
                                    lastParenthesis = false;
                                }
                                else
                                {
                                    throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_char(')')");
                                }
                            }
                            else if(tok.m_strToken.equals("["))
                            {
                            	lastParenthesis = false;

                                if(lastObj instanceof PrologObject)
                                    throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "operator_expected(" + ((PrologObject)lastObj).toString(m_opManager) + ")");

                                PrologObject term = translateTerm(STATE_SQUARE_BRACKET, lnReader);

                                //System.out.println("m_nTokCount " + m_nTokCount);
                                if (term == null)
                                {
                                    termStack.push(List.NIL);
                                }
                                else if(term instanceof List || term  instanceof Functor || term  instanceof Clause || !(term instanceof ConsCell))
                                {
                                    // atom
                                    termStack.push(new List(new ConsCell(term, null)));
                                }
                                else
                                {
                                    termStack.push(new List((ConsCell)term));
                                }
                            }
                            else if(tok.m_strToken.equals("]"))
                            {
                            	lastParenthesis = false;

                                if(nState == STATE_SQUARE_BRACKET || nState == STATE_PIPE)
                                {
                                    if(lastObj instanceof Operator)
                                    {
                                        Operator lastOp = (Operator) termStack.pop();
                                        termStack.push(Atom.createAtom(lastOp.getName()));
                                    }
                                    else if(((lastObj instanceof ConsCell) && !(lastObj instanceof Functor) && !(lastObj instanceof List)))
                                    {
                                        termStack.pop();
                                        termStack.push(new ConsCell((ConsCell)lastObj, null));
                                    }

                                    nState = STATE_NONE;
                                    bEnd = true;
                                }
                                else
                                {
                                    throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_char(']')");
                                }
                            }
                            else if(tok.m_strToken.equals("|"))
                            {
                            	lastParenthesis = false;

                                if(nState != STATE_SQUARE_BRACKET)
                                {
                                    if(lastObj instanceof PrologObject)
                                    {
                                        throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "operator_expected(" + ((PrologObject)lastObj).toString(m_opManager) + ")");
                                    }
                                    else
                                    {
                                        termStack.push(Atom.createAtom(tok.m_strToken));
                                    }
                                }
                                else
                                {
                                    if(lastObj instanceof Operator)
                                    {
                                        Operator lastOp = (Operator) termStack.pop();
                                        termStack.push(Atom.createAtom(lastOp.getName()));
                                    }
                                    else if(((lastObj instanceof ConsCell) && !(lastObj instanceof Functor) && !(lastObj instanceof List)))
                                    {
                                        termStack.pop();
                                        termStack.push(new ConsCell((ConsCell)lastObj, null));
                                    }

                                    // risolve quello che c'è a destra
                                    PrologObject objRight = translateTerm(STATE_PIPE, lnReader);
                                    // risolve quello che c'è a sinistra
                                    PrologObject objleft = resolveStack(termStack);

                                    if((objleft instanceof ConsCell) && !(objleft instanceof Functor) && !(objleft instanceof List))
                                    {
                                        ((ConsCell)objleft).setLast(objRight);
                                        termStack.push(objleft);
                                    }
                                    else
                                    {
                                        termStack.push(new ConsCell(objleft, objRight));
                                    }

                                    nState = STATE_NONE;
                                    bEnd = true;
                                }
                            }
                            else if(tok.m_strToken.equals("{"))
                            {
                            	lastParenthesis = false;

                                if(lastObj instanceof PrologObject)
                                    throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "operator_expected(" + ((PrologObject)lastObj).toString(m_opManager) + ")");

                                PrologObject term = translateTerm(STATE_SPECIAL_BRACKET, lnReader);
                                termStack.push(term);
                            }
                            else if(tok.m_strToken.equals("}"))
                            {
                            	lastParenthesis = false;

                                if(nState == STATE_SPECIAL_BRACKET)
                                {
                                    if(lastObj instanceof Operator)
                                    {
                                        Operator lastOp = (Operator) termStack.pop();
                                        termStack.push(Atom.createAtom(lastOp.getName()));
                                    }

                                    PrologObject term = resolveStack(termStack);
                                    if (term == null)
                                    {
                                        termStack.push(Atom.createAtom("{}"));
                                    }
                                    else
                                    {
	                                    termStack.push(new Functor("{}/1", new ConsCell(term, null)));
                                    }
                                    bEnd = true;
                                    nState = STATE_NONE;
                                }
                                else
                                {
                                    throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_char('}')");
                                    //throw syntaxError(tok.m_strToken, null);
                                }
                            }
                            else if(tok.m_strToken.equals("[]"))
                            {
                            	lastParenthesis = false;
                            	termStack.push(List.NIL);
                            }
                            else if(m_opManager.contains(tok.m_strToken))
                            {
                                Operator curOp = m_opManager.get(tok.m_strToken);
                                // cerca tra gli operatori
                                if(lastObj == null)
                                {
                                	if(curOp.getName().equals(",") && !quoted)
                                	{
                                		throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_term(" + curOp.getName() + ")");
                                	}
                                	else if(curOp.getPrefix() != null)
                                    {
                                        termStack.push(curOp.getPrefix());
                                    }
                                    else  // è considerato come atomo
                                    {
                                        termStack.push(Atom.createAtom(tok.m_strToken));
                                    }
                                }
                                else
                                    while(lastObj != null)
                                    {
                                        if(lastObj instanceof PrologObject)
                                        {
                                            if(curOp.getPostfix() != null)
                                            {
                                                if(termStack.size() == 1)
                                                {
                                                	if(lastObj instanceof Functor && (m_opManager.contains(((Functor)lastObj).getFriendlyName())))
                                                	{
                                                		Operator op = m_opManager.get(((Functor)lastObj).getFriendlyName());
                                                		if(curOp.getPrecedence() == op.getPrecedence() && curOp.isNonAssoc() && !lastParenthesis)
                                                		{
                                                			throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "not_assoc_operator(" + lastObj + ")");
                                                		}
                                                	}

                                                    PrologObject obj = resolveOperator(termStack, curOp.getPostfix());
                                                    termStack.push(obj);
                                                    lastObj = null;
                                                }
                                                else
                                                {
                                                    // estrae il postfix
                                                    curOp = curOp.getPostfix();

                                                    PrologObject lastTerm = (PrologObject)lastObj;
                                                    termStack.pop();
                                                    Operator lastOp = (Operator)termStack.peek();

                                                    if (curOp.getPrecedence() > lastOp.getPrecedence() ||
                                                            ((curOp.getPrecedence() == lastOp.getPrecedence()) && !lastOp.isNonAssoc() &&
                                                                 (curOp.isLeftAssoc() || !lastOp.isRightAssoc())))
                                                    {
                                                        // (a op2 b) op1
                                                        // tratta l'operator nello stack
                                                        termStack.pop();
                                                        termStack.push(lastObj);
                                                        PrologObject obj = resolveOperator(termStack, lastOp);
                                                        termStack.push(obj);

                                                        lastObj = obj;
                                                    }
                                                    else if (curOp.getPrecedence() < lastOp.getPrecedence() ||
                                                                 ((curOp.getPrecedence() == lastOp.getPrecedence()) && !lastOp.isNonAssoc() &&
                                                                      (!curOp.isLeftAssoc() || lastOp.isRightAssoc())))
                                                    {
                                                        // a op2 (b op1)
                                                        // tratta l'operator corrente
                                                        termStack.push(lastTerm);
                                                        PrologObject obj = resolveOperator(termStack, curOp);
                                                        termStack.push(obj);
                                                        lastObj = null;
                                                    }
                                                }
                                            }
                                            else if(curOp.getInfix() != null)
                                            {
//                                                System.out.println("infix " + curOp.getName());
                                                if(termStack.size() == 1)
                                                {
//                                                	if(lastObj instanceof Atom && ((Atom)lastObj).getName().equals(curOp.m_strName))
//                                                		throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_term(" + curOp.getName() + ")");

                                                    termStack.push(curOp.getInfix());
                                                    lastObj = null;
                                                }
                                                else
                                                {
                                                    curOp = curOp.getInfix();
                                                    termStack.pop();
                                                    Operator lastOp = (Operator)termStack.peek();

                                                    if((nState == STATE_ARG_LIST || nState == STATE_SQUARE_BRACKET) &&
                                                        (curOp.getName().equals(",")))
                                                    {
                                                        if(lastOp.getName().equals(","))
                                                        {
                                                            // mi trovo in arglist con ,
                                                            // inserisco l'op nello stack e vado avanti
                                                            termStack.push(lastObj);
                                                            termStack.push(curOp);

                                                            lastObj = null;
                                                        }
                                                        else
                                                        {
                                                            // compongo lastop senza badare alla precedenza
                                                            // (a lastop b) , c
                                                            // tratta l'operator nello stack
                                                            termStack.pop();
                                                            termStack.push(lastObj);
                                                            PrologObject obj = resolveOperator(termStack, lastOp);
                                                            termStack.push(obj);
                                                            lastObj = obj;
                                                        }
                                                    }
                                                    else if ((curOp.getPrecedence() > lastOp.getPrecedence()) ||
                                                            ((curOp.getPrecedence() == lastOp.getPrecedence()) && !lastOp.isNonAssoc() &&
                                                             (curOp.isLeftAssoc() || !lastOp.isRightAssoc())))
                                                    {
                                                        // (a lastOp b) curOp c
                                                        // tratta l'operator nello stack
                                                        termStack.pop();
                                                        termStack.push(lastObj);
//                                                        System.out.println("lastOp  " + lastOp);
                                                        PrologObject obj = resolveOperator(termStack, lastOp);
//                                                        System.out.println("Operator " + obj);
                                                        termStack.push(obj);
                                                        lastObj = obj;
//                                                        System.out.println("remainder " + lastObj);
                                                    }
                                                    else if ((curOp.getPrecedence() < lastOp.getPrecedence()) ||
                                                                 ((curOp.getPrecedence() == lastOp.getPrecedence()) && !lastOp.isNonAssoc() &&
                                                                      (!curOp.isLeftAssoc() || lastOp.isRightAssoc())))
                                                    {
                                                        termStack.push(lastObj);
                                                        termStack.push(curOp);
                                                        lastObj = null;
                                                    }
                                                }
                                            }
                                            else //prefix
                                            {
                                                throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_term(" + curOp.getName() + ")");
                                            }
                                        }
                                        else //if(lastObj instanceof Operator)
                                        {
                                            if(termStack.size() == 1)
                                            {
                                                // lastobj can be prefix only, otherwise it was transformet in Atom
                                            	if(lastObj instanceof Operator)
                                            	{
                                            		if(((Operator)lastObj).isPrefix() && (curOp.getPrecedence() == ((Operator)lastObj).getPrecedence()) && ((Operator)lastObj).isNonAssoc())
                                            			throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "not_assoc_operator(" + ((Operator)lastObj).getName() + ")");
                                            	}

                                                termStack.push(curOp);
                                                lastObj = null;
                                            }
                                            else  // stack not empty
                                            {
                                                Operator lastOp = (Operator)lastObj;//termStack.peek();
                                                if((nState == STATE_ARG_LIST || nState == STATE_SQUARE_BRACKET) &&
                                                    (curOp.getName().equals(",")))
                                                {
                                                    // mi trovo in arglist con ,
                                                    if(lastOp.getName().equals(","))
                                                    {
                                                        //curOp è atomo ,
                                                        //lastOp è operator
                                                    	if(quoted)
                                                    	{
                                                          Atom atom1 = Atom.createAtom(curOp.getName());
                                                          termStack.push(atom1);
                                                          lastObj = null;
                                                    	}
                                                    	else
                                                    		throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_term(" + curOp.getName() + ")");
                                                    }
                                                    else
                                                    {
                                                        //curOp è operator ,
                                                        //lastOp è atom
                                                        // versione precedente funzionante tranne che per
                                                        // write_canonical((a - -, b)).
                                                        // -(a, ','(-, b))
                                                        Atom atom1 = Atom.createAtom(lastOp.getName());
                                                        termStack.pop();
                                                        termStack.push(atom1);
                                                        termStack.push(curOp);
                                                        lastObj = null;
                                                    }
                                                }
                                                else if(lastOp.getPrefix() != null)
                                                {
                                                    if(curOp.getPostfix() != null)
                                                    {
                                                        curOp = curOp.getPostfix();
                                                        lastOp = lastOp.getPrefix();

                                                        if (curOp.getPrecedence() > lastOp.getPrecedence() ||
                                                            ((curOp.getPrecedence() == lastOp.getPrecedence()) && !lastOp.isNonAssoc() &&
                                                            (curOp.isLeftAssoc() || !lastOp.isRightAssoc())))
                                                        {
                                                            //curOp è atomo
                                                            //lastOp è operatore
                                                            termStack.pop();
                                                            termStack.push(Atom.createAtom(curOp.getName()));
                                                            PrologObject funct = resolveOperator(termStack, lastOp);
                                                            termStack.push(funct);
                                                            lastObj = null;
                                                        }
                                                        else
                                                        {
                                                            //curOp è operator
                                                            //lastOp è atom
                                                            Atom atom1 = Atom.createAtom(lastOp.getName());
                                                            termStack.pop();
                                                            termStack.push(atom1);
                                                            termStack.push(curOp);
                                                            lastObj = null;
                                                        }
                                                    }
                                                    else if(curOp.getPrefix() != null)
                                                    {
                                                        //System.out.println("qui");
                                                        termStack.push(curOp.getPrefix());
                                                        lastObj = null;
                                                    }
                                                    else // if(curOp.getInfix() != null)
                                                    {
                                                        termStack.push(curOp.getInfix());
                                                        lastObj = null;
                                                    }
                                                }
                                                else if(lastOp.getInfix() != null)
                                                {
                                                    lastOp = lastOp.getInfix();

                                                    termStack.pop();
                                                    Object lastLastObj = termStack.peek();
                                                    if(curOp.getPrefix() != null)
                                                    {
                                                        curOp = curOp.getPrefix();

//                                                        System.out.println("LasOP infix " + lastOp.m_strName + "CurOp Prefix " + curOp.m_strName);
                                                        // se curOp è prefisso va avanti
                                                        termStack.push(lastOp);
                                                        termStack.push(curOp);
                                                        lastObj = null;
                                                    }
                                                    else if(lastLastObj instanceof PrologObject)
                                                    {
                                                        // lastOp è un infix valido
                                                        // curOp è un atomo
                                                        termStack.push(lastOp);
                                                        termStack.push(Atom.createAtom(curOp.getName()));
                                                        lastObj = null;
                                                    }
                                                    else //if(lastLastObj instanceof Operator)
                                                    {
                                                        Operator lastLastOp = (Operator)lastLastObj;
                                                        if(lastLastOp.getPrefix() != null)
                                                        {
                                                            lastLastOp = lastLastOp.getPrefix();

                                                            // lastOp è un infix valido
                                                            // lastLastObj è un atomo
                                                            // curOp è un atomo
                                                            //termStack.push(lastLastObj);
                                                            termStack.pop();  // estraggo lastlastobj
                                                            termStack.push(Atom.createAtom(lastLastOp.getName()));
                                                            termStack.push(lastOp);
                                                            termStack.push(Atom.createAtom(curOp.getName()));
                                                            //PrologObject funct = resolveOperator(termStack, lastOp);
                                                            //termStack.push(funct);
                                                            lastObj = null;
                                                        }
                                                        else if(lastLastOp.getInfix() != null)
                                                        {
                                                            lastLastOp = lastLastOp.getInfix();

                                                            if ((curOp.getPrecedence() > lastOp.getPrecedence() ||
                                                                ((curOp.getPrecedence() == lastOp.getPrecedence()) && !lastOp.isNonAssoc() &&
                                                                 (curOp.isLeftAssoc() || !lastOp.isRightAssoc())))  ||
                                                                    ((nState == STATE_ARG_LIST || nState == STATE_SQUARE_BRACKET) && lastOp.getName().equals(",")))
                                                            {
                                                                // lastOp ha precedenza maggiore
                                                                //curOp è atomo
                                                                //lastlastOp è atomo
                                                                //lastOp è operatore
                                                                termStack.pop();  // estraggo lastlastobj
                                                                termStack.push(Atom.createAtom(lastLastOp.getName()));
                                                                termStack.push(lastOp);
                                                                termStack.push(Atom.createAtom(curOp.getName()));
                                                                //PrologObject funct = resolveOperator(termStack, lastOp);
                                                                //Functor funct = makeFunctor(Atom.createAtom(lastOp.getName()), new ConsCell(Atom.createAtom(lastLastOp.getName()), new ConsCell(Atom.createAtom(curOp.getName()), null)));
                                                                //termStack.push(funct);
                                                                lastObj = null;
                                                            }
                                                            else
                                                            {
                                                                // curOp ha precedenza maggiore
                                                                //curOp è operator
                                                                //lastOp è atom
                                                                Atom atom1 = Atom.createAtom(lastOp.getName());
                                                                termStack.pop();
                                                                termStack.push(atom1);
                                                                termStack.push(curOp);
                                                                lastObj = null;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }

                                lastParenthesis = false;
                            }
                            else
                            {
                                if(lastObj instanceof PrologObject)
                                {
                                    throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "operator_expected(" + ((PrologObject)lastObj).toString(m_opManager) + ")");
                                }
                                else
                                {
                                    termStack.push(Atom.createAtom(tok.m_strToken));
                                }

                                lastParenthesis = false;
                            }


                        }
                        break;
                }

                if(tok.m_nType == PrologTokenizer.TOKEN_WHITESPACE)
                    bWhiteSpace = true;
                else
                    bWhiteSpace = false;
            }

            if(nState != STATE_NONE)
                throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_eof");

            PrologObject obj = resolveStack(termStack);

            if(obj != null)
            	obj.setPosition(line + 1, column, position);

            return obj;

        }
        catch(IOException ex)
        {
            throw new JIPJVMException(ex);
        }
        catch (NumberFormatException ex)
        {
            //ex.printStackTrace();
            throw new JIPJVMException(ex);
        }
        catch (ClassCastException ex)
        {
            ex.printStackTrace();
            if(tok != null)
                throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_term(" + tok.m_strToken + ")");
                //throw syntaxError(tok.m_strToken, null);
            else
                throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_term(unknown)");
                //throw syntaxError("unknown", null);
        }
    }

    private final PrologObject resolveOperator(Stack termStack, Operator op) throws JIPSyntaxErrorException
    {
        //        System.out.println(termStack.peek());
        PrologObject obj1 = (PrologObject)termStack.pop();

        if(op.isPrefix())//prefix
        {
        	if((op.getName().equals("-") || op.getName().equals("+")) && obj1 instanceof Expression)
        	{
        		return Expression.createNumber(op.getName() + obj1.toString());
        	}
        	else if((!(obj1 instanceof ConsCell)) || obj1 instanceof List || obj1 instanceof Functor || ((ConsCell)obj1).getHeight() != 1)
            {
                obj1 = new ConsCell(obj1, null);
            }

            return makeFunctor(Atom.createAtom(op.getName()), (ConsCell)obj1);
        }
        else if(op.isPostfix())// postfix and prefix
        {
            if((!(obj1 instanceof ConsCell)) || obj1 instanceof List || obj1 instanceof Functor || ((ConsCell)obj1).getHeight() != 1)
            {
                obj1 = new ConsCell(obj1, null);
            }

            return makeFunctor(Atom.createAtom(op.getName()), (ConsCell)obj1);
        }
        else if(op.isInfix() && (termStack.size() > 0))
        {
            Object obj2 = termStack.pop();
//          if(((obj1 instanceof ConsCell)) && !(obj1 instanceof List) && !(obj1 instanceof Functor) && (((ConsCell)obj1).getHeight() == 1))
//          {
//              obj1 =  ((ConsCell)obj1).getHead();
//          }
//
//          if(((obj2 instanceof ConsCell)) && !(obj2 instanceof List) && !(obj2 instanceof Functor) && (((ConsCell)obj2).getHeight() == 1))
//          {
//              obj2 =  ((ConsCell)obj2).getHead();
//          }

            if(obj2 instanceof Operator && ((Operator)obj2).getPrefix() != null)
            {
                if(op.getPrefix() != null && (termStack.size() == 0))
                {
                    //sono entrambi prefissi
                    if((!(obj1 instanceof ConsCell)) || obj1 instanceof List || obj1 instanceof Functor || ((ConsCell)obj1).getHeight() != 1)
                    {
                        obj1 = new ConsCell(obj1, null);
                    }

                    Functor funct1 = (Functor)makeFunctor(Atom.createAtom(op.getName()), (ConsCell)obj1);

                    return makeFunctor(Atom.createAtom(((Operator)obj2).getName()), new ConsCell(funct1, null));

                }

//                System.out.println("obj2 " + obj2);
                // controlla la precedenza e l'associatività
                if ((termStack.size() == 0) ||
                        op.getName().equals(",") ||
                        op.getPrecedence() > ((Operator)obj2).getPrecedence() ||
                        ((op.getPrecedence() == ((Operator)obj2).getPrecedence()) && /*!((Operator)obj2).isNonAssoc() &&*/
                             (op.isLeftAssoc() || !((Operator)obj2).isRightAssoc())))
                {
//                    // (a op2 b) op1
//                    // tratta l'operator nello stackop(100, fx, fx).
//	                // trasforma in atomo
	                obj2 = Atom.createAtom(((Operator)obj2).getName());
                }
                else
                {
                    if((!(obj1 instanceof ConsCell)) || obj1 instanceof List || obj1 instanceof Functor || ((ConsCell)obj1).getHeight() != 1)
                    {
                        obj1 = new ConsCell(obj1, null);
                    }

                    if(((Operator)obj2).getInfix() != null)
                    	return makeFunctor(Atom.createAtom(((Operator)obj2).getName()), new ConsCell(Atom.createAtom(op.getName()), obj1));
                    else
                        throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_term(" + obj1 + ")");
                }
            }

            if(op.getName().equals(","))
            {
                if((!(obj1 instanceof ConsCell)) || obj1 instanceof List || obj1 instanceof Functor)
                {
                    obj1 = new ConsCell(obj1, null);
                }

                return new ConsCell((PrologObject)obj2, obj1);
            }
            else
            {
                if((!(obj1 instanceof ConsCell)) || obj1 instanceof List || obj1 instanceof Functor || ((ConsCell)obj1).getHeight() != 1)
                {
                    obj1 = new ConsCell(obj1, null);
                }

                return makeFunctor(Atom.createAtom(op.getName()), new ConsCell((PrologObject)obj2, obj1));
            }
        }
        else
        {
            throw new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), "unexpected_term(" + op.getName() + ")");
            //throw syntaxError(op.getName(), null);
        }
    }

    private final PrologObject resolveStack(Stack termStack) throws JIPSyntaxErrorException
    {
        if(termStack.isEmpty())
            return null;

        Object obj;
        obj = termStack.pop();

        while(!termStack.isEmpty())
        {
            if(obj instanceof Operator)
            {
                PrologObject term = resolveOperator(termStack, (Operator)obj);
                termStack.push(term);
            }
            else //obj instanceof PrologObject
            {
//                if(!termStack.isEmpty())
//                {
                    PrologObject term = (PrologObject)obj;//termStack.pop();
                    Operator op = (Operator)termStack.pop();
                    termStack.push(term);
                    term = resolveOperator(termStack, op);
                    termStack.push(term);
//                }
            }

            obj = termStack.pop();
        }

        return (PrologObject)obj;
    }

    private final static PrologObject makeFunctor(Atom funct, ConsCell params)
    {
        if(params != null)
        {

            String strFunctor = funct.getName() + "/" + params.getHeight();
//          System.out.println(strFunctor);
            if(BuiltInFactory.isBuiltIn(strFunctor))
            {
                return new BuiltInPredicate(strFunctor, params);
            }
            else if(strFunctor.equals("./2"))
            {
                return makeList(params);
                //return new List(params.getHead(), params.getTail().getHead());
            }
            else if(strFunctor.equals(",/2"))
            {
                return makeCons(params);
            }
            else
            {
//              System.out.println("2 " + strFunctor);
                Functor func =  new Functor(strFunctor, params);
//              System.out.println("3 " + strFunctor);
                return func;
            }
        }
        else
        {
            String strFunctor = funct.getName() + "/0";

            if(BuiltInFactory.isBuiltIn(strFunctor))
            {
                return new BuiltInPredicate(strFunctor, null);
            }
            else
            {
                return new Functor(strFunctor, null);
            }
        }
    }

    private final static List makeList(ConsCell params)
    {
        if(params.getTail() != null)
        {
            return new List(params.getHead(), ((ConsCell)params.getTail()).getHead());
        }
        else
        {
            return new List(params.getHead(), null);
        }
    }

    private final static ConsCell makeCons(ConsCell params)
    {
        if(params.getTail() != null)
        {
            final PrologObject obj = ((ConsCell)params.getTail()).getHead();
            if(obj instanceof ConsCell && !(obj instanceof List) && !(obj instanceof Functor))
                return new ConsCell(params.getHead(), obj);
            else
                return new ConsCell(params.getHead(), new ConsCell(obj, null));
        }
        else
        {
            return new ConsCell(params.getHead(), null);
        }
    }

    public int getLineNumber()
    {
    	return m_lnReader.getLineNumber();
    }

/*
    private final JIPSyntaxErrorException syntaxError(Object term, String strTerm)
    {
        String strMsg;
        if(term == null)
            strMsg = "unexpected";
        else
            if(strTerm != null)
                strMsg = "unexpected_term_before(" + term.toString() + ")";
            else
                strMsg = "unexptected_term (" + term.toString() + ")";

        return new JIPSyntaxErrorException(m_strFileName, (m_lnReader.getLineNumber() + 1), strMsg);

    }
*/
}
