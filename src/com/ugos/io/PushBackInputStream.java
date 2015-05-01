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

package com.ugos.io;

import java.io.*;

public class PushBackInputStream extends PushbackInputStream// FilterInputStream
{
    private boolean m_bSkipLF;
    private int m_nLineNumber;
    private int m_nColNumber;
    private int m_nRead;
//    private int m_nLastChar = -1;
//    private boolean m_bPushedBack;

    private ByteArrayOutputStream m_outs;

    public PushBackInputStream(final InputStream ins)
    {
        super(ins);
        m_nColNumber = -1;
//        m_nLastChar = -1;
//        m_bPushedBack = false;
        m_bSkipLF = false;
        m_nLineNumber = 1;
        m_nRead = 0;
    }

    public final int getLineNumber()
    {
        return m_nLineNumber;
    }

    public final int getColumnNumber()
    {
        return m_nColNumber;
    }

    public final int getRead()
    {
        return m_nRead;
    }

    public final int read() throws IOException
    {
        int c;
//        if(m_bPushedBack)
//        {
////            System.out.println("Pushed Back " + (char)m_nLastChar);
//            c = m_nLastChar;
//            m_bPushedBack = false;
//        }
//        else
//        {
            c = super.read();
            if (m_bSkipLF)
            {
                if (c == '\n')
                {
                    m_nColNumber++;
                    m_nRead++;
                    c = super.read();
                }
                m_bSkipLF = false;
            }


            switch (c)
            {
                case '\r':
                    m_bSkipLF = true;
                case '\n':      /* Fall through */
                    m_nLineNumber++;
                    m_nColNumber = -1;
                    c = '\n';
            }

//            m_nLastChar = c;
//        }

        if(c > -1)
        {
            m_nColNumber++;
            m_nRead++;
        }

        return c;
    }

    @Override
    public void unread(int c) throws IOException
    {
    	super.unread(c);
    	if(c == '\n')
    		m_nLineNumber--;
    }
//    public void pushback()
//    {
//        if(!m_bPushedBack && m_nLastChar != -1)
//        {
//            m_nColNumber--;
//            m_nRead--;
//        }
//
//        m_bPushedBack = true;
//    }
}

