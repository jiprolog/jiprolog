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

import com.ugos.jiprolog.engine.*;

public class JIPUserException extends JIPRuntimeException
{
    private JIPTerm m_term;

    public JIPUserException(final JIPTerm term)
    {
        super(3000, "User Exception");
        m_term = term;
    }

    public JIPUserException(final PrologObject term)
    {
    	this(JIPTerm.getJIPTerm(term));
    }

    public JIPTerm getTerm()
    {
        return m_term;
    }

}

