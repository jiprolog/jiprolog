/*****************************************
 * 27/03/2003
 *
 * Copyright (C) 1999-2003 Ugo Chirico
 * http://www.ugochirico.com
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
 *****************************************/

package com.ugos.JIProlog.extensions.system;

import com.ugos.JIProlog.engine.*;

import java.util.*;
import java.io.*;

public class Time8 extends JIPXCall
{
    public final boolean unify(final JIPCons input, Hashtable varsTbl)
    {
        JIPTerm millis = input.getNth(1);

        // check if input is a variable
        if (millis instanceof JIPVariable)
        {
            // try to extract the term
            if(!((JIPVariable)millis).isBounded())
            {
                throw new JIPParameterUnboundedException(1);
            }
            else
            {
                //extracts the term
                millis = ((JIPVariable)millis).getValue();
            }
        }
        if (!(millis instanceof JIPNumber))
        {
            throw new JIPParameterTypeException(1, JIPParameterTypeException.INTEGER);
        }

        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(new Date((long)((JIPNumber)millis).getDoubleValue()));

        final JIPNumber hour   = JIPNumber.create(calendar.get(GregorianCalendar.HOUR_OF_DAY));
        final JIPNumber min    = JIPNumber.create(calendar.get(GregorianCalendar.MINUTE));
        final JIPNumber sec    = JIPNumber.create(calendar.get(GregorianCalendar.SECOND));
        final JIPNumber milsec = JIPNumber.create(calendar.get(GregorianCalendar.MILLISECOND));
        final JIPNumber year   = JIPNumber.create(calendar.get(GregorianCalendar.YEAR));
        final JIPNumber month  = JIPNumber.create(calendar.get(GregorianCalendar.MONTH) + 1);
        final JIPNumber day    = JIPNumber.create(calendar.get(GregorianCalendar.DAY_OF_MONTH));

        //JIPCons date  = JIPCons.create(year, JIPCons.create(month, JIPCons.create(day, JIPCons.create(hour, JIPCons.create(min, JIPCons.create(sec, JIPCons.create(milsec, null)))))));
        //JIPCons pdate = JIPCons.create(getParam(1), JIPCons.create(getParam(2), JIPCons.create(getParam(3), JIPCons.create(getParam(4), JIPCons.create(getParam(5), JIPCons.create(getParam(6), JIPCons.create(getParam(7), null)))))));

        return
            input.getNth(2).unify(year, varsTbl) &&
            input.getNth(3).unify(month, varsTbl) &&
            input.getNth(4).unify(day, varsTbl) &&
            input.getNth(5).unify(hour, varsTbl) &&
            input.getNth(6).unify(min, varsTbl) &&
            input.getNth(7).unify(sec, varsTbl) &&
            input.getNth(8).unify(milsec, varsTbl);
    }

    public boolean hasMoreChoicePoints()
    {
        return false;
    }
}

