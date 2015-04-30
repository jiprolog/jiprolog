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

import com.ugos.io.PushBackInputStream;

class ParserReader extends Reader
{
//    private boolean m_bSkipLF;
//    private int m_nLineNumber;
    private int m_nLastChar = -1;
//    private boolean m_bPushedBack;
    private boolean m_bEOF;
//    private int m_nRead;

    private PushBackInputStream m_ins;

    //private ByteArrayOutputStream m_outs;

    public ParserReader(final PushBackInputStream ins)
    {
        m_ins = ins;

//        m_nLastChar = -1;
//        m_bPushedBack = false;
//        m_bSkipLF = false;
//        m_nLineNumber = 0;
        m_bEOF = false;
//        m_nRead = 0;
    }

    public final int getLineNumber()
    {
        return m_ins.getLineNumber();
    }

    public final int getRead()
    {
        return m_ins.getRead();
    }

    public final int read() throws IOException
    {
        int c;
//        if(m_bPushedBack)
//        {
//            //System.out.println("Pushed Back " + (char)m_nLastChar);
//            c = m_nLastChar;
//            m_bPushedBack = false;
//        }
//        else
//        {
            c = m_ins.read();
//            if (m_bSkipLF)
//            {
//                if (c == '\n')
//                {
//                    m_nRead++;
//                    c = m_ins.read();
//                }
//                m_bSkipLF = false;
//            }
//
//
//            switch (c)
//            {
//                case '\r':
//                    m_bSkipLF = true;
//                case '\n':      /* Fall through */
//                    m_nLineNumber++;
//                c = '\n';
//            }

//              switch (c)
//              {
//                  case '\r':
//                      m_bSkipLF = true;
//                      m_nLineNumber++;
//                      break;
//
//                  case '\n':      /* Fall through */
//                      if(m_bSkipLF)
//                          m_bSkipLF = false;
//                      else
//                          m_nLineNumber++;
//                      break;
//
//                  default:
//                      m_bSkipLF = false;
//              }
//
              m_nLastChar = c;
//        }

        if( c == -1)
            m_bEOF = true;
//        else
//            m_nRead++;

        return c;
    }

    public void unread(int c)
    {
    	try {
			m_ins.unread(c);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }
//    public void pushback()
//    {
//    	try {
//			m_ins.unread(m_nLastChar);
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//    }
//        if(!m_bPushedBack && !m_bEOF)
//            m_nRead--;
//
//        m_bPushedBack = true;
//    }

    public boolean eof()
    {
        return m_bEOF;
    }

    public void close() throws IOException
    {
        m_ins.close();
    }

    /* (non-Javadoc)
     * @see java.io.Reader#read(char[], int, int)
     */
    public int read(char[] arg0, int arg1, int arg2) throws IOException
    {
       int c = 0;
       int start = arg1;
       while(arg2 > 0 &&  (c = read()) != -1)
       {
           arg0[arg1] = (char)c;
           arg1++;
           arg2--;
       }

       return arg1 - start;
    }


}

