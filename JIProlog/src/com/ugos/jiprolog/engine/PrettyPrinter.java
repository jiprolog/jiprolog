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

import java.util.Enumeration;
import java.util.Hashtable;

import com.ugos.util.*;

final class PrettyPrinter extends Object
{
    //private static OperatorManager s_opManager = new OperatorManager();

    private static final char[] ESCAPE = {'a', 'b', 't', 'n', 'v', 'f', 'r'};
    private static String Q_CHARS = "\\()[].,`{}\"";

    public static final String printTerm(final PrologObject obj, final OperatorManager opManager, final boolean bQ)
    {
    	Hashtable<String, Variable> varTable = new Hashtable<String, Variable>();

    	return print(obj, opManager, bQ, varTable);
    }

    private static final String print(final PrologObject obj, final OperatorManager opManager, final boolean bQ, Hashtable<String, Variable> varTable)
    {
        if (obj instanceof Atom)
        {
            return printAtom(obj, opManager, bQ);
        }
        else if(obj instanceof Functor)
        {
            return printFunctor(obj, opManager,bQ, varTable);
        }
        else if(obj instanceof Clause)
        {
            return printClause(obj, opManager, bQ, varTable);
        }
        else if(obj instanceof Expression)
        {
            return printExpression(obj, bQ);
        }
        else if(obj instanceof List)
        {
            return printList(obj, opManager, bQ, varTable);
        }
        else if(obj instanceof ConsCell)
        {
            return printCons(obj, opManager,bQ, varTable);
        }
        else if(obj instanceof Variable)
        {
            return printVariable(obj, opManager, bQ, varTable);
        }
        else
        {
            throw new JIPRuntimeException("Invalid type cannot be printed");//return "";
        }
    }

    private static final String printAtom(final PrologObject obj, final OperatorManager opManager, final boolean bQ)
    {
        return printAtomString(((Atom)obj).getName(), opManager,bQ);
    }

    private static final String printAtomString(final String strAtom, final OperatorManager opManager, final boolean bQ)
    {
        if(opManager == null || bQ)  // canonical
        {
            if(strAtom.length() == 0)
                return "''";

            if(strAtom.equals("{}"))
                return "{}";

            if(strAtom.equals("[]"))
                return "[] ";

            boolean bQuoted = false;

            if(Q_CHARS.indexOf(strAtom) > -1)
                bQuoted = true;
//                return "'" + strAtom + "'";


            String strRet = "";

            // se comincia con una maiuscola occorre l'apice
            if(PrologTokenizer.UPPERCASE_CHARS.indexOf(strAtom.charAt(0)) > -1)
                bQuoted = true;

            // se comincia con un numero occorre l'apice
            char c = strAtom.charAt(0);
            if(c < 58 && c > 47)
                bQuoted = true;

            // se contiene almeno uno spazio occorre l'apice
            if(strAtom.indexOf(' ') > -1)
                bQuoted = true;

            boolean bSimpleFound = false;
            boolean bSpecialFound = false;
            boolean bSingletonFound = false;

            // tratta i caratteri speciali
            for(int i = 0; i < strAtom.length(); i++)
            {
                switch(strAtom.charAt(i))
                {
                    case '\'':
                        strRet+= "\\'";
                        bQuoted = true;
                        break;

                    case '\\':
                        bSpecialFound = true;

//                        if(bSimpleFound)
                            strRet+= "\\\\";
//                        else
//                        	strRet += "\\";

                        bQuoted = true;
                        break;

                    default:
                        c = strAtom.charAt(i);
                        if(PrologTokenizer.LOWERCASE_CHARS.indexOf(c) > -1 ||
                           PrologTokenizer.UPPERCASE_CHARS.indexOf(c) > -1 ||
                           (c < 58 && c > 47))
                        {
                            bSimpleFound = true;
                            strRet+= c;
                        }
                        else if(PrologTokenizer.SINGLETON_CHARS.indexOf(c) > -1)
                        {
                            bSingletonFound = true;
                            strRet+= c;
                        }
                        else if(c <= 13 && c >= 7)
                        {
                            bQuoted = true;
                            strRet+= "\\" + ESCAPE[c - 7];;
                        }
                        else if(c < ' ')
                        {
                            bQuoted = true;
                            strRet+= "\\x" + ValueEncoder.byteToHexString((byte)c);
                            Integer.toString(c);
                            //strRet+= "~" + (char)(c + '@');
                        }
                        else
                        {
                            bSpecialFound = true;
                            strRet+= c;
                        }
                }

//	            System.out.println("" + bSimpleFound + " " + bSingletonFound + " " + bSpecialFound);

//	            System.out.println("Quoted " + bQuoted);
            }

            bQuoted = bQuoted ||
                    (bSimpleFound && bSingletonFound) ||
                    (bSpecialFound && bSingletonFound) ||
                    (bSimpleFound && bSpecialFound);

            if(bQuoted)
            {
                return "'" + strRet + "'";
            }
            else
            {
                return strRet;
            }
        }
        else
        {
            return strAtom;
        }
    }

    private static final String printFunctor(final PrologObject obj, final OperatorManager opManager, final boolean bQ, Hashtable<String, Variable> varTable)
    {
        final Functor funct = (Functor)obj;

        if(opManager != null && opManager.contains(funct.getFriendlyName()))
        {
            final Operator op = opManager.get(funct.getFriendlyName());
            if((funct.getArity() == 1 && (op.getPrefix() != null || op.getPostfix() != null)) ||
                   (funct.getArity() == 2 && (op.getInfix() != null)))
                return printOperator(obj, opManager, bQ, varTable);
        }


        // Estrae il nome
        final String strFunctor = funct.getFriendlyName();

        // Estrae i parametri
        ConsCell params = funct.getParams();

        // Costruisce la stringa
        if(params == null)
        {
            return printAtomString(strFunctor, opManager, bQ);//;
        }
        else
        {
            // tratta il caso speciale {}
            if(strFunctor.equals("{") && funct.getArity() == 1)
            {
                if(params.getHead() instanceof Functor && ((Functor)params.getHead()).getFriendlyName().equals("}"))
                {
                    ConsCell par = ((Functor)params.getHead()).getParams();
                    if(par != null && par != ConsCell.NIL)
                        return "{}(" + printParams(par, opManager, bQ, varTable) + ")";
                    else
                        return "{}";
                }
            }

            String strParams = printParams(params, opManager, bQ, varTable);
//          String strParams = "";
//          while (params != null)
//          {
//              strParams += print(params.getHead());
//              params = (ConsCell)params.getTail();
//              if(params != null && params != List.NIL && params != ConsCell.NIL)
//                  strParams += ", ";
//          }

            //return strFunctor + "(" + strParams + ")";
            return printAtomString(strFunctor, opManager, bQ) + "(" + strParams + ")";
        }
    }

    private static final String printOperator(final PrologObject obj, final OperatorManager opManager, final boolean bQ, Hashtable<String, Variable> varTable)
    {
        final Functor oper = (Functor)obj;

        // Estrae il nome
        String strOper = oper.getFriendlyName();

        // Estrae i parametri
        final ConsCell params = oper.getParams();
        if(params == null)
            return printAtomString(strOper, opManager, bQ);;

        int nArity = params.getHeight();

        final Operator op = opManager.get(strOper);

        PrologObject head = params.getHead();
        String strFirst;
//        if(head instanceof List || head instanceof Functor || !(head instanceof ConsCell))
//        {
            strFirst = print(head, opManager,bQ, varTable);
//        }
//        else
//        {
//            strFirst = printCons(head);
//        }

        if (nArity == 1 && op.getPrefix() != null)
        {
            return printAtomString(strOper, opManager, bQ) + " " + strFirst;
        }
        else if (nArity == 1 && op.getPostfix() != null)
        {
            return strFirst + " " + printAtomString(strOper, opManager, bQ);
        }
        else //if (op.isInfix())
        {
            String strSecond;
            PrologObject tail = BuiltIn.getRealTerm(params.getTail());

            //System.out.println(tail.getClass());
            if(tail instanceof List || tail instanceof Functor || !(tail instanceof ConsCell))
            {
                //System.out.println("print");
                strSecond = print(tail, opManager, bQ, varTable);
                //System.out.println(strSecond);
            }
            else
            {
                //System.out.println("printCons");
                strSecond = print(((ConsCell)tail).getHead(), opManager, bQ, varTable);
                //System.out.println(strSecond);
            }

            //if(params.getHead() instanceof ConsCell)
            //    strFirst = printCons(params.getHead());
            //else
//                strFirst = print(params.getHead());

            //if(((ConsCell)params.getTail()).getHead() instanceof ConsCell)
            //    strSecond = printCons(((ConsCell)params.getTail()).getHead());
            //else

//                strSecond  = print(((ConsCell)params.getTail()).getHead());

            //return print(params.getHead()) + " " + strOper + " " + print(((ConsCell)params.getTail()).getHead());
            return strFirst + " " + printAtomString(strOper, opManager, bQ) + " " + strSecond;
        }
    }

    private static final String printClause(final PrologObject obj, final OperatorManager opManager, final boolean bQ, Hashtable<String, Variable> varTable)
    {
        // Estrae il nome
        final PrologObject head = ((Clause)obj).getHead();
        final ConsCell tail = (ConsCell)((Clause)obj).getTail();

        if(tail == null)
            return print(head, opManager, bQ, varTable) + ".";
        else
        {
            String strHead = print(head, opManager, bQ, varTable);
            String strBody = print(tail.getHead(), opManager, bQ, varTable);

//          String strBody = "";
//          String strHead = print(head);
//          ConsCell params = tail;
//
            //            while (params != null)
            //            {
            //                strBody += print(params.getHead());
            //                params = (ConsCell)params.getTail();
            //                if(params != null && params != List.NIL && params != ConsCell.NIL)
            //                    strBody += ", ";
            //            }

            //String strBody = print(tail);

            return strHead + ":-" + strBody + ".";
        }
    }

    private static final String printExpression(final PrologObject obj, final boolean bQ)
    {
        final double dVal = ((Expression)obj).getValue();
        final int nVal = (int)dVal;

        if(((Expression)obj).isInteger())
            return Integer.toString(nVal);
        else
            return Double.toString(dVal);
    }

    private static final String printList(final PrologObject obj, final OperatorManager opManager, final boolean bQ, Hashtable<String, Variable> varTable)
    {
        if(opManager == null) // canonical
        {
            if(((List)obj).isNil())
                return "[]";
            else if(((List)obj).getTail() != null)
                return "'.'(" + print(((List)obj).getHead(), null, bQ, varTable) + ", " + print(((List)obj).getTail(), null, bQ, varTable) + ")";
            else
                return "'.'(" + print(((List)obj).getHead(), null, bQ, varTable)+ ", [])";
        }
        else
        {
            return "[" + printCons(obj, opManager, bQ, varTable) + "]";
        }
    }

    private static final String printParams(final ConsCell cons, final OperatorManager opManager, final boolean bQ, Hashtable<String, Variable> varTable)
    {
        // Stampa di una lista
        String strParams = "";
        PrologObject term = cons;
        PrologObject head = null;

        if(cons.getHead() == null)
        {
            return "";
        }

        while (term != null)
        {
            head = ((ConsCell)term).getHead();

            if(head != null)
            {
                strParams += print(head, opManager, bQ, varTable);

                term = ((ConsCell)term).getTail();

                if(term != null)
                {
                    head = ((ConsCell)term).getHead();

                    if(head != null)
                        strParams += ", " ;
                }
            }
            else
            {
                term = null;
            }
        }

        return strParams ;
    }
//    private static final String printConsCell(final PrologObject obj)
//    {
//        final ConsCell cons = (ConsCell)obj;
//        final PrologObject obj1 = BuiltIn.getRealTerm(cons.getTail());
//        if(obj1 != null && obj1 != ConsCell.NIL)
//            return "("  + printCons(obj) + ")";
//        else
//            return printCons(obj);
//    }

    private static final String printCons(final PrologObject obj, final OperatorManager opManager, final boolean bQ, Hashtable<String, Variable> varTable)
    {
        // Stampa di una lista
        final ConsCell cons = (ConsCell)obj;
        String strConsCell = "";
        PrologObject term = cons;
        PrologObject head = null;

        if(cons.getHead() == null)
        {
            return "";
        }
        else if(opManager != null)
        {
            while (term != null)
            {
                head = ((ConsCell)term).getHead();

                if(head != null)
                {
                    if(head instanceof Variable && ((Variable)head).isBounded())
                        head = BuiltIn.getRealTerm(head);

//                    System.out.println(head.getClass());
                    if(head instanceof ConsCell && !(head instanceof Functor) && !(head instanceof List) && !(head instanceof Clause))
                        strConsCell += "(" + printCons(head, opManager, bQ, varTable) + ")";
                    else
                        strConsCell += print(head, opManager, bQ, varTable);

                    term = ((ConsCell)term).getTail();

                    if(term instanceof Variable)
                    {
                        Variable var = (Variable)term;
                        term = BuiltIn.getRealTerm(term);

                        if(term == null)
                        {
                            strConsCell += "|" + print(var, opManager, bQ, varTable);
                        }
                    }

                    if(term instanceof ConsCell)
                    {
                        head = ((ConsCell)term).getHead();

                        if(head instanceof Variable && ((Variable)head).isBounded())
                            head = BuiltIn.getRealTerm(head);

                        if(head != null)
                            strConsCell += ", " ;
                    }
                    else if(term != null)
                    {
                        strConsCell += "|" + print(term, opManager, bQ, varTable);
                        term = null;
                    }
                }
                else
                {
                    term = null;
                }
            }

            return strConsCell ;
        }
        else
        {
            // Stampa di una lista canonica

            strConsCell = "";

            head = ((ConsCell)term).getHead();
            if(head instanceof Variable && ((Variable)head).isBounded())
                head = BuiltIn.getRealTerm(head);

            term = ((ConsCell)term).getTail();
            if(term instanceof Variable && ((Variable)term).isBounded())
                term = BuiltIn.getRealTerm(term);

            if(head != null)
            {
//                if(head instanceof ConsCell && !(head instanceof Functor) && !(head instanceof List) && !(head instanceof Clause))
//                {
//                    strConsCell += "','(" + printCons(head, opManager) + ")";
//                }
//                else
//                {
                    strConsCell += print(head, opManager, bQ, varTable);// + ", ";
//                }
            }

            if(term instanceof ConsCell)
            {
                head = ((ConsCell)term).getHead();

                if(head instanceof Variable && ((Variable)head).isBounded())
                    head = BuiltIn.getRealTerm(head);

                if(head != null)
                {
                    return "','(" + strConsCell + ", " + print(term, opManager, bQ, varTable) + ")";
                }
                else
                {
                    return strConsCell;
                }

//                if(((ConsCell)term).getTail() == null || ((ConsCell)term).getTail() == ConsCell.NIL)
//                {
//                    return "','(" + strConsCell + ", " + print(((ConsCell)term).getHead(), opManager, bQ) + ")";
//                }
//                else
//                {
//                    return "','(" + strConsCell + ", " + printCons(term, opManager, bQ) + ")";
//                }
            }
            else if(term != null)
            {
                return "','(" + strConsCell +  ", " + print(term, opManager, bQ, varTable) + ")";
            }
            else
            {
                return strConsCell;
            }
        }
    }

    private static final String printVariable(final PrologObject obj, final OperatorManager opManager, final boolean bQ, Hashtable<String, Variable> varTable)
    {
        // Stampa di una variabile
        final Variable var = ((Variable)obj).lastVariable();

        final PrologObject object = var.getObject();

        if(var.cyclic() && varTable.containsKey(var.getName()))
//        if(varTable.containsKey(var.getName()))
        {
            return var.getName();
        }
        else if(object == null)
            return "_" + Integer.toString(var.getAddress());// + ":" + Integer.toString(var.hashCode()); /* + var.getName() + "?";*/
            //return var.getName()  + " = " + "_" + Integer.toString(var.getAddress());// + ":" + Integer.toString(var.hashCode()); /* + var.getName() + "?";*/
        else
            varTable.put(var.getName(), var);
            return print(object, opManager, bQ, varTable);
                //return var.getAddress() + "." + print(var.getObject());
                //return var.getName() + "=" + print(object, opManager);

//            	return print(object, opManager, true);
//        }
    }
}

