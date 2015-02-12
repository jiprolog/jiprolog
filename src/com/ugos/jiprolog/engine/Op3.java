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

import java.util.*;

final class Op3 extends BuiltIn
{
    public final boolean unify(final Hashtable varsTbl)
    {
        final PrologObject prec  = getRealTerm(getParam(1));
        final PrologObject assoc = getRealTerm(getParam(2));
        final PrologObject op    = getRealTerm(getParam(3));

        try
        {
            Expression prec1 = (Expression)prec;
            Atom assoc1      = (Atom)assoc;
            Atom op1         = (Atom)op;

//            System.out.println(prec1);
//            System.out.println(assoc1);
//            System.out.println(op1);

            if(op1.getName().equals(".") ||
               op1.getName().equals(","))
            {
            	throw new JIPPermissionException("modify", "operator", op1.getName());
//                throw JIPRuntimeException.create(48, op1.getName());
            }

            if(getJIPEngine().getGlobalDB().isSystem(op1.getName()))
            	throw new JIPPermissionException("modify", "operator", op1.getName());
//                throw JIPRuntimeException.create(14, op1.getName());

            if(prec1.getValue() > 1200 || prec1.getValue() < 0)
            	throw new JIPDomainException("operator_priority", op1.getName());
//                throw JIPRuntimeException.create(16,op1.getName());

            final String strAssoc = assoc1.getName();
            if(!(strAssoc.equals("xfx") || strAssoc.equals("xfy") || strAssoc.equals("yfx") ||
                 strAssoc.equals("yfy") || strAssoc.equals("fx") || strAssoc.equals("fy") ||
                 strAssoc.equals("xf") || strAssoc.equals("yf")))
            	throw new JIPDomainException("operator_specifier", op1.getName());
//                throw JIPRuntimeException.create(17, op1.getName() + " - " + strAssoc);



            if(prec1.getValue() == 0)
                getJIPEngine().getOperatorManager().remove(assoc1.getName(), op1.getName());
            else
                getJIPEngine().getOperatorManager().put((int)prec1.getValue(), assoc1.getName(), op1.getName());
        }
        catch(NullPointerException ex)
        {
             throw new JIPParameterUnboundedException();
        }
        catch(ClassCastException ex)
        {
             throw new JIPParameterTypeException();
        }

        return true;
    }
}
