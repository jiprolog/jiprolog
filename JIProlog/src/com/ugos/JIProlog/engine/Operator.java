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


final class Operator
{
    public int    m_nPrecedence;
    public String m_strAssoc;
    public String m_strName;
    public Operator m_suppOp;

    public Operator(final int nPrec, final String strAssoc, final String strName)
    {
        m_nPrecedence = nPrec;
        m_strAssoc = strAssoc;
        m_strName = strName;
    }

    final Operator getSupplementaryOp()
    {
        return m_suppOp;
    }

    final Operator getInfix()
    {
        if(isInfix())
            return this;
        else
            if((m_suppOp != null) && m_suppOp.isInfix())
                return m_suppOp;
            else
                return null;
    }

    final Operator getPrefix()
    {
        if(isPrefix())
            return this;
        else
            if((m_suppOp != null) && m_suppOp.isPrefix())
                return m_suppOp;
            else
                return null;
    }

    final Operator getPostfix()
    {
        if(isPostfix())
            return this;
        else
            if((m_suppOp != null) && m_suppOp.isPostfix())
                return m_suppOp;
            else
                return null;
    }

    final boolean isBinary()
    {
        return isInfix();
    }

    final boolean isUnary()
    {
        return !isInfix();
    }

    final int getArity()
    {
        if(isInfix())
            return 2;
        else
            return 1;
    }
    final int getPrecedence()
    {
        return m_nPrecedence;
    }

    final String getAssoc()
    {
        return m_strAssoc;
    }

    final String getName()
    {
        return m_strName;
    }

    final boolean isInfix()
    {
        return m_strAssoc.equals("yfx") || m_strAssoc.equals("xfx") || m_strAssoc.equals("xfy") || m_strAssoc.equals("yfy");
    }

    final boolean isPrefix()
    {
        return m_strAssoc.equals("fx") || m_strAssoc.equals("fy");
    }

    final boolean isPostfix()
    {
        return m_strAssoc.equals("xf") || m_strAssoc.equals("yf");
    }

    final boolean isRightAssoc()
    {
        return (m_strAssoc.charAt(m_strAssoc.length() - 1) == 'y');
    }

    final boolean isLeftAssoc()
    {
        return (m_strAssoc.charAt(0) == 'y');
    }

    public final String toString()
    {
        return getName();
    }
}
