/*
 * 23/04/2014
 *
 * Copyright (C) 1999-2014 Ugo Chirico - http://www.ugochirico.com
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

//import java.io.Serializable;
import java.util.Hashtable;

final class Expression extends PrologObject //implements Serializable
{
    final static long serialVersionUID = 300000004L;

    private double m_dValue;
    private boolean floating = false;

    public static Expression createNumber(final double dNum)
    {
        return new Expression(dNum);
    }

    public static Expression createNumber(final String strNum)
    {
    	Expression expr = new Expression(Double.valueOf(strNum).doubleValue());
    	if(strNum.contains("."))
    		expr.floating = true;

    	return expr;
    }

    private Expression(final double dNum)
    {
        m_dValue   = dNum;
        floating = (int)dNum != dNum;
    }

    public final PrologObject copy(final boolean flat, final Hashtable<Variable, PrologObject> varTable)
    {
        return this;
    }

    public final boolean _unify(final PrologObject obj, final Hashtable table)
    {
        //System.out.println("Match Expression");

        if(obj instanceof Expression)
        {
            return (((Expression)obj).m_dValue == m_dValue);
        }
        else if(obj instanceof Variable)
        {
            return obj._unify(this, table);
        }
        else
            return false;
    }

    public final void clear()
    {
    }

    public static final Expression compute(final PrologObject expr)
    {
        PrologObject exp = expr;
//      System.out.println(exp);  //DBG
//      System.out.println(exp.getClass());  //DBG
        if(exp instanceof Variable)
            exp = ((Variable)exp).getObject();

        if(exp instanceof Expression)
        {
            return (Expression)exp;
        }
        if(exp instanceof Atom)
        {
            final String strExp = ((Atom)exp).getName();

            if (strExp.equals("rand"))
            {
                return Expression.createNumber(Math.random());
            }
            else if (strExp.equals("e"))
            {
                return Expression.createNumber(Math.E);
            }
            else if (strExp.equals("pi"))
            {
                return Expression.createNumber(Math.PI);
            }
            else if (strExp.equals("cputime"))
            {
                return Expression.createNumber(System.currentTimeMillis());
            }
            else
            {
                throw new JIPParameterTypeException(new Functor(Atom.createAtom(strExp)), JIPParameterTypeException.EVALUABLE);
                //throw JIPRuntimeException.create(2, ((Atom)exp).getName() + " doesn't evaluate to a numeric expression");
                //return computeCalculus();
            }
        }
        else if(exp instanceof Functor)
        {
            double dblVal;

            try
            {
                final Functor  func       = (Functor) exp;
                final String   strFunName = func.getFriendlyName();
                final ConsCell params     = func.getParams();

//                System.out.println(strFunName);
//                System.out.println(params);
//
                switch(func.getArity())
                {
                    case 1:
                        PrologObject exp1 = params.getHead();
                        double dVal1 = Expression.compute(exp1).m_dValue;
                        if (strFunName.equals("-"))
                        {
                            dblVal = - dVal1;
                        }
                        else if (strFunName.equals("+"))
                        {
                            dblVal = dVal1;
                        }
                        else if (strFunName.equals("sin"))
                        {
                            dblVal =  Math.sin(dVal1);
                        }
                        else if (strFunName.equals("cos"))
                        {
                            dblVal =  Math.cos(dVal1);
                        }
                        else if (strFunName.equals("tan"))
                        {
                            dblVal =  Math.tan(dVal1);
                        }
                        else if (strFunName.equals("asin"))
                        {
                            dblVal =  Math.asin(dVal1);
                        }
                        else if (strFunName.equals("acos"))
                        {
                            dblVal =  Math.acos(dVal1);
                        }
                        else if (strFunName.equals("atan"))
                        {
                            dblVal =  Math.atan(dVal1);
                        }
                        else if (strFunName.equals("log"))
                        {
                            dblVal =  Math.log(dVal1);
                        }
                        else if (strFunName.equals("exp"))
                        {
                            dblVal =  Math.exp(dVal1);
                        }
                        else if (strFunName.equals("int") || strFunName.equals("integer") || strFunName.equals("round"))
                        {
                            dblVal =  Math.rint(dVal1);
                        }
                        else if (strFunName.equals("ceil") || strFunName.equals("ceiling"))
                        {
                            dblVal =  Math.ceil(dVal1);
                        }
                        else if (strFunName.equals("floor") || strFunName.equals("truncate") || strFunName.equals("rnd"))
                        {
                            dblVal =  Math.floor(dVal1);
                        }
                        else if (strFunName.equals("float"))
                        {
                            dblVal =  dVal1;
                        }
                        else if (strFunName.equals("float_fractional_part"))
                        {
                            dblVal =  dVal1 - (int)dVal1;
                        }
                        else if (strFunName.equals("float_integer_part"))
                        {
                            dblVal =  dVal1 - (int)dVal1;
                        }
                        else if (strFunName.equals("abs"))
                        {
                            dblVal =  Math.abs(dVal1);
                        }
                        else if (strFunName.equals("sqrt"))
                        {
                            dblVal =  Math.sqrt(dVal1);
                        }
                        else if (strFunName.equals("sign"))
                        {
                            if(dVal1 > 0)
                                dblVal = 1;
                            else
                                dblVal = -1;
                        }
                        else if(strFunName.equals("\\")) // bitwise negation
                        {
                            dblVal =  ~(int)dVal1;
                        }
                        else if(strFunName.equals("random")) // random
                        {
                            dblVal =  Math.random() * dVal1;
                        }
                        else
                        {
                            throw new JIPEvaluationException(JIPEvaluationException.undefined);//.create(2, strFunName + " is unknown");
                        }

                        break;

                    case 2:
                        exp1 = params.getHead();
                        final PrologObject exp2 = ((ConsCell)params.getTail()).getHead();
                        dVal1 = Expression.compute(exp1).m_dValue;
                        final double dVal2 = Expression.compute(exp2).m_dValue;

                        if(strFunName.equals("+"))
                        {
                            dblVal =  dVal1 + dVal2;
                        }
                        else if(strFunName.equals("-"))
                        {
                            dblVal =  dVal1 - dVal2;
                        }
                        else if(strFunName.equals("/"))
                        {
                            dblVal =  dVal1 / dVal2;
                        }
                        else if(strFunName.equals("//"))
                        {
                            dblVal =  ((int)dVal1 / (int)dVal2);
                        }
                        else if(strFunName.equals("*"))
                        {
                            dblVal =  dVal1 * dVal2;
                        }
                        else if (strFunName.equals("pow") || strFunName.equals("**") || strFunName.equals("^"))
                        {
                            dblVal =  Math.pow(dVal1, dVal2);
                        }
                        else if (strFunName.equals("min"))
                        {
                            dblVal =  Math.min(dVal1, dVal2);
                        }
                        else if (strFunName.equals("max"))
                        {
                            dblVal =  Math.max(dVal1, dVal2);
                        }
                        else if (strFunName.equals("mod") || strFunName.equals("rem"))
                        {
                            dblVal =  dVal1 % dVal2;
                        }
                        else if(strFunName.equals("/\\"))  // bitwise and
                        {
                            dblVal =  (int)dVal1 & (int)dVal2;
                        }
                        else if(strFunName.equals("\\/")) // bitwise or
                        {
                            dblVal =  (int)dVal1 | (int)dVal2;
                        }
                        else if(strFunName.equals("<<")) // left shift
                        {
                            dblVal =  (int)dVal1 << (int)dVal2;
                        }
                        else if(strFunName.equals("xor")) // xor
                        {
                            dblVal =  (int)dVal1 ^ (int)dVal2;
                        }
                        else if(strFunName.equals(">>")) // right shift
                        {
                            dblVal =  (int)dVal1 >> (int)dVal2;
                        }
                        else if(strFunName.equals("div")) // DIV
                        {
                            dblVal =  (int)(dVal1 - dVal1 % dVal2) / dVal2;
                        }

                        else
                        {
                        	throw new JIPEvaluationException(JIPEvaluationException.undefined);
//                            throw JIPRuntimeException.create(2, strFunName + " is unknown");
                        }
                        break;

                    default:
                    	throw new JIPEvaluationException(JIPEvaluationException.undefined);
//                        throw JIPRuntimeException.create(2, strFunName + "/" + Integer.toString(func.getArity()) + " doesn't evaluate to a numeric expression");
                }

                return Expression.createNumber(dblVal);
            }
            catch(JIPRuntimeException ex)
            {
                throw ex;
            }
            catch(ClassCastException ex)
            {
                throw new JIPParameterTypeException();
            }
            catch(NullPointerException ex)
            {
            	throw new JIPEvaluationException(JIPEvaluationException.undefined);
//                throw JIPRuntimeException.create(2, "Wrong number of arguments in expression");
            }
        }
        else if(exp instanceof ConsCell && !(exp instanceof List))
        {
            if(((ConsCell)exp).getHeight() != 1)
                throw new JIPParameterTypeException();

            return compute(((ConsCell)exp).getHead());
        }
        else if (exp == null)
        {
            throw new JIPParameterUnboundedException();
        }
        else
        {
        	throw new JIPParameterTypeException(-1, JIPParameterTypeException.EVALUABLE);
        }
    }

    public final double getValue()
    {
        //System.out.println(m_Operator);
        return m_dValue;
    }

    protected final boolean lessThen(final PrologObject obj)
    {
        if(obj instanceof Expression)
        {
        	if(floating && !((Expression)obj).floating)
        		return true;
        	else if(((Expression)obj).floating && !floating)
        		return false;
        	else if(m_dValue < ((Expression)obj).m_dValue)
            {
            	return true;
            }
        }
        else if(obj instanceof Atom)
        {
        	return true;
        }
        else if(obj.unifiable(List.NIL))
        {
        	return true;
        }
        else if(obj instanceof Variable)
            if(((Variable)obj).isBounded())
                return lessThen(((Variable)obj).getObject());

        return false;
    }

    public final boolean isInteger()
    {
        return !floating;//(int)m_dValue == m_dValue;
    }

    @Override
    public boolean termEquals(PrologObject obj)
    {
        if(obj instanceof Expression)
        {
            return m_dValue == ((Expression)obj).m_dValue && floating == ((Expression)obj).floating;
        }
        else if(obj instanceof Variable && ((Variable)obj).isBounded())
            return termEquals(((Variable)obj).getObject());

        return false;
    }
}
