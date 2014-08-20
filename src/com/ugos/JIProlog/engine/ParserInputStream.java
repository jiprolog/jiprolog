/**
 * JIPInputStream.java
 *
 * @author Created by Omnicore CodeGuide
 */

package com.ugos.JIProlog.engine;

import java.io.*;

class ParserInputStream extends FilterInputStream
{
    private boolean m_bSkipLF;
    private int m_nLineNumber;
    private int m_nLastChar = -1;
    private boolean m_bPushedBack;
    private boolean m_bEOF;
    private int m_nRead;
            
    private ByteArrayOutputStream m_outs;
    
    public ParserInputStream(final InputStream ins)
    {
        super(ins);
        
        m_nLastChar = -1;
        m_bPushedBack = false;
        m_bSkipLF = false;
        m_nLineNumber = 0;
        m_bEOF = false;
        m_nRead = 0;
    }

    public final int getLineNumber()
    {
        return m_nLineNumber;
    }
    
    public final int getRead()
    {
        return m_nRead;
    }

    public final int read() throws IOException
    {
        int c;
        if(m_bPushedBack)
        {
//            System.out.println("Pushed Back " + (char)m_nLastChar);
            c = m_nLastChar;
            m_bPushedBack = false;
        }
        else
        {
            c = super.read();
            if (m_bSkipLF)
            {
                if (c == '\n')
                {
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
                c = '\n';
            }

            m_nLastChar = c;
        }
        
        if( c == -1)
            m_bEOF = true;
        else
            m_nRead++;
        
        return c;
    }
    
    public void pushback()
    {
        if(!m_bPushedBack && !m_bEOF)
            m_nRead--;
        
        m_bPushedBack = true;
    }
    
    public boolean eof()
    {
        return m_bEOF;
    }
}

