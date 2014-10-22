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

import java.io.*;
import java.util.*;

final class Encoding1 extends Notify2
{
    public final boolean unify(final Hashtable varsTbl)
    {
        PrologObject param = getParam(1);
        if (param instanceof Variable)
        {
            if (!((Variable)param).isBounded())
            {
                if(getJIPEngine().getEncoding() == null)
                    throw JIPRuntimeException.create(26, null);
                
                return param.unify(Atom.createAtom(getJIPEngine().getEncoding()), varsTbl);
            }
            else
            {
                param = ((Variable)param).getObject();
            }
        }
        
        String strEncoding;
        if (param instanceof Atom)
        {
            strEncoding = ((Atom)param).getName();
        }
        else if (param instanceof PString)
        {
            strEncoding = ((PString)param).getString();
        }
        else
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.ATOM_OR_STRING);
        }
        
        getJIPEngine().setEncoding(strEncoding);
    
        notifyEvent(JIPEvent.ID_ENCODINGCHANGED, Atom.createAtom(strEncoding));
        
        return true;
    }
 }


