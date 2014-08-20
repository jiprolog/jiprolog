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

package com.ugos.JIProlog.engine;

import java.util.*;

final class Ground1 extends BuiltIn
{
    public final boolean unify(final Hashtable varsTbl)
    {
        return checkVariable(getParam(1));
    }
        
    private static final boolean checkVariable(final PrologObject term)
    {
        //System.out.println(term);
        
        if(term instanceof ConsCell)
        {
            return checkVariable(((ConsCell)term).getHead()) &&
                   checkVariable(((ConsCell)term).getTail());
        }
        else if(term instanceof Variable)
        {
            if(((Variable)term).isBounded())
                return checkVariable(((Variable)term).getObject());
            else
                return false;
        }
        else
            return true;
    }
}
